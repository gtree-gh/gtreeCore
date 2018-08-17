# Try to check in how far a proposed strategy profile described by a set of
# rules constitutes a Nash equilibrium in a vg game.
#
# Goal: No need to describe the whole game tree, such that also large games
# can be analysed.
example.check.vg.eq = function() {
  restore.point("check.vg.eq")
  setwd("D:/libraries/gtree/myproject")
	gameId = "UltimatumGame"
	#gameId = "UG2"
	vg = get.vg(gameId = gameId)
  rules = list(
    action.rule("offer",1),
    action.rule("accept",offer >= 2)
  )

  check.vg.rules.eq(vg, rules)


}

check.vg.rules.eq = function(vg, rules, util.funs = NULL, check.all=FALSE, order=c("random","top-down","bottom-up")[check.all+1], progress.bar=TRUE) {
  restore.point("check.vg.rules.eq")

  # Play according to specified rules
  rp = play.vg.rules(vg, rules, make.stage.li=TRUE)

  # Compute expected utility of rule play
  rules.util = vg.play.expected.utility(rp$play,util.funs = NULL)


  stage.li = rp$stage.li

  # Compute a list that contains for each stage
  # a data frame of all possible deviations at that stage
  dev.li = lapply(seq_along(vg$stages), vg.stage.deviations, stage.li=stage.li, vg=vg)

  # Create a table that references to all possible deviations
  dev.ref = bind_rows(lapply(dev.li, function(dev) {
    if (is.null((dev))) return(NULL)
    as_data_frame(list(stage.num = dev$.stage.num,row=1:NROW(dev)))
  }))

  # Set order in which deviations are checked
  if (order=="random") {
    dev.ref = sample_frac(dev.ref)
  } else if (order=="bottom-up") {
    dev.ref = dev.ref[rev(seq_len(NROW(dev.ref))),]
  }

  if (progress.bar)
    pb = utils::txtProgressBar(min=0,max=NROW(dev.ref),initial = 0)


  for (i in seq_len(NROW(dev.ref))) {
    if (progress.bar)
      utils::setTxtProgressBar(pb,i)

    dev = dev.li[[ dev.ref$stage.num[i] ]][dev.ref$row[i],]
    dev.play = play.vg.rules.with.deviation(dev=dev,vg=vg, rules=rules, stage.li=stage.li)
    dev.util = vg.play.expected.utility(dev.play, util.funs)

    profitable = dev.util[dev$.player] > rules.util[dev$.player]
    if (profitable) {
      if (progress.bar) close(pb)
      dev$.gain = dev.util[dev$.player] - rules.util[dev$.player]
      return(nlist(stage.li=invisible(stage.li),dev.play=dev.play, rules.util, dev.util, rules.play=rp$play, dev,is.eq=FALSE))
    }

  }
  if (progress.bar) close(pb)
  return(nlist(stage.li=invisible(stage.li),rules.util,rules.play=rp$play,is.eq=TRUE))
}

action.rule = function(var, formula, condition=NULL, stage=NULL) {
  formula=substitute(formula)
  condition=substitute(condition)
  nlist(var, formula, condition, stage)
}


play.vg.rules.with.deviation = function(dev, rules, stage.li, vg, stage.num=dev$.stage.num, make.stage.li=FALSE, extra.par=NULL,...) {
  restore.point("play.vg.rules.with.deviation")

  # Actions for which we might deviate
  actions = names(dev)[!str.starts.with(names(dev),".")]

  # Original play under rules
  play = stage.li[[stage.num]]

  # Rows corresponding to information set in which deviation
  # takes place
  replace.rows = which(play$.info.set == dev$.info.set)

  # Change chosen action to the deviation
  for (action in actions) {
    play[replace.rows,action] = dev[[action]]
  }

  # Now play the deviation
  play.vg.rules(vg=vg, rules=rules,play=play,start.stage = stage.num+1, make.stage.li=make.stage.li,extra.par=extra.par,...)
}

vg.stage.deviations = function(stage.num, play=stage.li[[stage.num]], vg, stage.li) {
  vg.stage.action.sets.by.info.set(stage.num, play, vg, only.deviations=TRUE)
}

vg.stage.action.sets.by.info.set = function(stage.num, play, vg, only.deviations=FALSE) {
  restore.point("vg.stage.action.sets.by.info.set")

  if (is.null(play)) return(NULL)

  play$.stage.num = stage.num
  ise.play = play %>% group_by(.info.set) %>%
    summarise_all(first)
  stage = vg$stages[[stage.num]]


  res = bind_rows(lapply(stage$actions, function(action) {
    df = remove.cols(ise.play, action$name)
    eval.set.to.df(action$set,df=df,var=action$name)
  }))

  res = res[,c(".stage.num", ".info.set", ".player", names(stage$actions))]
  if (only.deviations) {
    res = anti_join(res, ise.play, by=colnames(res))
    return(res)
  }

  ise.play$.rule.play = TRUE
  res = left_join(res, ise.play, by=colnames(res)) %>%
    mutate(
      .rule.play = ifelse(is.na(.rule.play), FALSE, TRUE)
    )

  res = res[,c(".stage.num", ".info.set",".player", ".rule.play", names(stage$actions))]
  res
}

play.vg.rules  = function(vg, rules=vg$rules, extra.par = list(), make.stage.li = FALSE, add.info.sets = make.stage.li, start.stage=1, play=stage.li[[start.stage]], stage.li=NULL) {
  restore.point("play.vg.rules")
  rules.vars = sapply(rules, function(rule) rule$var)

  if (is.null(play))
    play = as_data_frame(c(list(.prob=1),vg$params, extra.par))

  numPlayers = vg$params$numPlayers


  add.info.sets = make.stage.li
  if (make.stage.li) {
    stage.play.li = vector("list", length(vg$stages))
  }

  # In order to check for equilibrium
  # we need to compute information sets
  # and add the to stage.li
  if (add.info.sets) {
    # Initialize know.li elements with one column
    know.li = lapply(1:numPlayers, function(i) {
      as_data_frame(list(numPlayers=rep(TRUE, NROW(play))))
    })
  }

  stage.num = start.stage-1
  while (stage.num < length(vg$stages)) {
    stage.num = stage.num+1
    stage = vg$stages[[stage.num]]

    if (add.info.sets) {
      play$.info.set = NA
    }

    if (!is.empty(stage$condition)) {
      play$.omit = !eval.on.df(stage$condition, play)
      omit.play = filter(play,.omit)

      if (add.info.sets){
        omit.know.li = lapply(1:numPlayers, function(i) {
          know.mat[play$.omit,,drop=FALSE]
        })
        know.li = lapply(1:numPlayers, function(i) {
          know.mat[!play$.omit,,drop=FALSE]
        })
      }

      play = filter(play,!.omit)
    } else {
      omit.play = NULL
      omit.know.li = NULL
    }

    if (NROW(play)==0) {
      stop("PLAY HAS ZERO ROWS: NEED TO IMPLEMENT HANDLING")
    }

    # Compute transformations
    for (trans in stage$compute) {
      play[[trans$name]] = eval.on.df(trans$formula, play)
    }

    # Add observed variables to know.li and compute info.sets
    if (add.info.sets) {
      restore.point("jnsjfnjnfjd")
      play = add.call.players.to.df(call=stage$player, df=play, numPlayers=numPlayers)
      if (!all(is.empty(stage$observe))) {
        for (i in 1:numPlayers) {
          know.mat = know.li[[i]]
          if (!is.character(stage$observe))
            stop(paste0("Stage ", stage$name, " has specified a formula for observe. We can currently only deal with a fixed set of variables specified in observe. Best split up the stage in several stage, with different conditions and a fixed observed variables for each stage."))
          for (var in stage$observe) {
            rows = play[[paste0(".player_",i)]]
            if (!has.col(know.mat,var)) know.mat[[var]] = FALSE
            know.mat[rows,var] = TRUE
          }
          know.li[[i]] = know.mat
        }
      }

      # Create info set index for this level
      play$.info.set = compute.info.sets(play, know.li, stage.num, just.index=TRUE)
      play$.ROW = seq_len(NROW(play))
    }

    # Run moves of nature
    for (nature in stage$nature) {
      play = eval.randomVar.to.df(nature$set, nature$prob,df = play,var=nature$name,kel=NULL)
      play$.prob = play$prob * play$.move.prob
    }

    # Expand know.li given that additional rows have been generated
    if (add.info.sets & length(stage$nature)>0) {
      know.li = lapply(1:numPlayers, function(i) {
        know.li[[i]][play$.ROW,,drop=FALSE]
      })
    }




    # Play actions
    for (action in stage$actions) {
      var = action$name
      for (rule in rules[rules.vars == var]) {
        if (is.character(rule$stage))
          if (rule$stage != stage$name) next

        if (is.numeric(rule$stage))
          if (rule$stage != stage$name) next

        if (!is.empty(rule$condition)) {
          rows = which(eval.on.df(rule$condition, play))
          if (length(rows)==0) next
          if (length(rows)==NROW(play)) rows=NULL
        } else {
          rows = NULL
        }

        # Evaluate rule for all p
        if (is.null(rows)) {
          play[[var]] = eval.on.df(rule$formula, play)
        } else {
          play[[var]][rows] = eval.on.df(rule$formula, play[rows,,drop=FALSE])
        }
      }
    }
    if (add.info.sets) {
      for (i in 1:numPlayers) {
        know.mat = know.li[[i]]
        player.rows = play$.player == i
        if (!has.col(know.mat,var)) know.mat[[var]] = FALSE
        know.mat[player.rows, var] = TRUE
        know.li[[i]] = bind_rows(know.mat, omit.know.li[[i]])
      }
    }
    if (NROW(omit.play)>0) {
      play = bind_rows(play, omit.play)
    }

    if (make.stage.li & length(stage$actions)>0) {
      stage.play.li[[stage.num]] = play
    }

  }
  play = remove.cols(play, c(paste0(".player_",1:numPlayers), ".player",".ROW",".info.set"))
  if (make.stage.li) {
    return(list(stage.li=stage.play.li, play=play))
  }

  return(play)

}

add.call.players.to.df =  function(call,df,numPlayers) {
  restore.point("add.call.players.to.df")
  # compute player set for each node
  if (NROW(df)==0) return(df)
  players = 1:numPlayers

  # fixed player sets
  if (!is(call, "call") & !is(call,"name")) {
    for (i in players) {
      df[[paste0(".player_",i)]] = i %in% call
    }
    if (length(call)>0)
      df$.player = call[1]

    return(df)
  }

  # players is a call
  df$.ROW.FOR.PLAYER = seq.int(NROW(df))
  # reduce df to unique combination of used variables
  vars = find.variables(call)

  if (length(vars)==0) {
    stop("Please only use a formula in players if it depends on some earlier defined parameter or variable.")
  }

  if (length(unknown <- setdiff(vars, colnames(df)))>0) {
    stop("Your observe formula depends on variables, which have not been defined earlier.")
  }



  sdf = as_data_frame(unique(df[,vars,drop=FALSE]))

  for (i in players) {
    sdf[[paste0(".player_",i)]] = FALSE
    df[[paste0(".player_",i)]] = FALSE
  }

  for (row in seq.int(NROW(sdf))) {
    rdf = sdf[row,,drop=FALSE]
    call.players = eval(call,rdf)
    if (length(call.players)==0) next
    if (length(unknown <- setdiff(call.players, players))>0) {
        stop("Your evaluated formula states to observe variable(s), which have not been defined earlier.")
      }
    cols = paste0(".player_",call.players)

    # get rows in original df
    mdf = left_join(rdf,df, by=vars)
    rows = mdf$.ROW.FOR.PLAYER

    # Set all found players to TRUE
    df[rows,cols] = TRUE

    # Set player just to first player
    # if an action is chosen, there
    # must be a unique player in the stage
    df[rows,".player"] = players[1]
  }
  return(df)
}


add.util.cols = function(df, util.funs, players=NULL) {
  if (is.null(players)) {
    numPlayers = str.starts.with(colnames(play),"payoff_")
    players = 1:numPlayers
  }
  for (i in players) {
    util.fun = util.funs[[min(i, length(util.funs))]]
    if (is.character(util.fun)) util.fun = parse(text=util.fun)
    col = paste0("util_",i)
    df[[col]] = eval.on.df(util.fun, df)
  }
  df
}

vg.play.expected.utility = function(play, util.funs=NULL) {
  numPlayers = sum(str.starts.with(colnames(play),"payoff_"))

  if (is.null(util.funs)) {
    util.cols = paste0("payoff_",1:numPlayers)
  } else {
    play = add.util.cols(play, util.funs, players=1:numPlayers)
    util.cols = paste0("util_",1:numPlayers)
  }

  utils = unlist(lapply(util.cols, function(col) {
    sum(play$.prob*play[[col]])

  }))
  names(utils) = util.cols
  utils
}
