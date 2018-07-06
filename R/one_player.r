# Internal algorithm to solve tg games in which just one player makes a decision
#
# Can be used to quickly...
# 1. ...find best replies
# 2. ...solve for first best solution

example.solve.single.player.tg = function() {
  setwd("D:/libraries/gtree/myproject")

	gameId = "UG3"
	tg.org = get.tg(gameId = gameId, never.load = !FALSE)
	eq.li = get.eq(tg = tg.org, never.load=TRUE)
  eq = eq.li[[1]]

  tg = make.best.reply.tg(tg.org, eq, player=2, tremble.prob = 1/100)
  eq = compute.single.player.eq(tg, player=2, find.all.eq=FALSE)
  eq.li = compute.single.player.eq(tg, player=2, find.all.eq=TRUE)

}

compute.single.player.eq = function(tg, player=1, info.set.probs = NULL, return.just.info.set.probs=FALSE, find.all.eq=FALSE) {
  restore.point("compute.single.player.eq")

  util.cols = c(paste0("util_",1:tg$numPlayers))

  # We write one equilibrium into info.set.probs
  if (is.null(info.set.probs)) {
    row = which.max(tg$ise.df$.info.set.move.ind.start)
    total.moves = tg$ise.df$.info.set.move.ind.start[row] + tg$ise.df$.num.moves[row]-1
    info.set.probs = rep(0, total.moves)
  }

  if (find.all.eq) {
    info.set.probs = matrix(info.set.probs,1)
  }


  # Find index of first action level
  # we don't need to perform any computation before
  first.action.level = min(tg$action.levels)

  # Outcomes that will be reduced via backward induction
  odf = tg$oco.df

  # Go through levels backwards
  lev.num = length(tg$lev.li)+1

  while(lev.num > first.action.level) {
    lev.num = lev.num-1
    lev = tg$lev.li[[lev.num]]
    lev.df = lev$lev.df

    join.cols = intersect(tg$lev.vars, colnames(lev.df))
    # Outcomes that are not reached at this level
    # add them later again to odf
    omitted.odf = anti_join(odf, lev.df, by=join.cols)
    # Join utilities to lev.df
    lev.df = inner_join(lev.df, odf[,c(join.cols, util.cols)],
      by=join.cols)

    if (lev$type=="nature") {

      # Compute expected utilities
      # by grouping over .node.ind
      odf  = lev.df %>%
        group_by_at(c(setdiff(join.cols, lev$var),".node.ind")) %>%
        summarize_at(util.cols, funs(weighted.mean(., .move.prob))) %>%
        ungroup() %>%
        select(-.node.ind)

    } else if (lev$type=="action") {
      # Compute for each information set the
      # total probability over all nodes that it will
      # be reached
      lev.df = lev.df %>% group_by(.info.set.ind) %>%
        mutate(.info.set.prob = sum(.prob * (.move.ind==1))) %>%
        ungroup() %>%
        # Compute the probability of each node given that
        # its information set is reached
        # TO DO: If an information set is reached with zero prob,
        # we have to assign some probabilities
        mutate(.node.prob.in.info.set = .prob / .info.set.prob)

      # Now compute for each .move.ind in each information set
      # the expected utilities
      temp.df  = lev.df %>%
        group_by(.info.set.ind, .move.ind) %>%
        summarize_at(util.cols, funs(weighted.mean(., .node.prob.in.info.set))) %>%
        ungroup()

      # Set relevant decision utility to the utility of the player
      temp.df[[".util"]] = temp.df[[paste0("util_", player)]]

      # Find a single equilibrium
      single.eq.df = temp.df %>% group_by(.info.set.ind) %>%
        mutate(.best.move = .move.ind[which.max(.util)]) %>%
        filter(.move.ind == .best.move) %>%
        select(-.util, -.best.move) %>%
        ungroup()

      # Join on equilibrium back to lev.df
      # Note: single.eq.df has one row per info.set
      # the result of the join will have one row
      # per information set node.
      odf = inner_join(single.eq.df, lev.df[,c(join.cols,".info.set.ind",".move.ind",".info.set.move.ind")], by=c(".info.set.ind",".move.ind"))

      # Set equilibrium probabilities of the selected
      # moves to 1
      if (!find.all.eq) {
        info.set.probs[unique(odf$.info.set.move.ind)] = 1
      } else {
        restore.point("make.all.eq")

        all.eq.df = temp.df %>% group_by(.info.set.ind) %>%
          mutate(.is.best.move = .util == max(.util)) %>%
          filter(.is.best.move)

        df = inner_join(
          all.eq.df[,c(".info.set.ind",".move.ind")],
          lev.df[,c(".info.set.ind",".move.ind",".info.set.move.ind")],
          by=c(".info.set.ind",".move.ind")
        )
        grid.li = (df %>% group_by(.info.set.ind) %>%
          summarize(moves = list(unique(.info.set.move.ind))))$moves
        eq.grid = as.matrix(expand.grid(grid.li))

        # Each row of eq.grid corresponds to a combination
        # of equilibrium moves at this level
        prob.li = lapply(seq_len(NROW(eq.grid)), function(row) {
          mat = info.set.probs
          mat[,as.integer(eq.grid[row,])] = 1
          mat
        })
        info.set.probs = do.call(rbind, prob.li)
      }


      # We are already finished and can skip
      # further computation for the loop
      if (lev.num <= first.action.level) break

      odf = odf %>% select(-.info.set.ind, -.move.ind, -.info.set.move.ind)
    }
    # rbind initially omitted rows to the new odf
    if (NROW(omitted.odf)>0) {
      odf = bind_rows(odf, omitted.odf)
    }
  }

  if (return.just.info.set.probs)
    return(info.set.probs)



  if (!find.all.eq) {
    eq.mat = ceq.to.eq.mat(ceq=info.set.probs,tg = tg,efg.move.inds = NULL)
    return(eq.mat)
  } else {
    eq.li = lapply(seq_len(NROW(eq.grid)), function(row) {
      eq.mat = ceq.to.eq.mat(ceq=info.set.probs[row,],tg = tg,efg.move.inds = NULL)
    })
    return(eq.li)
  }

}

single.player.reduce.nature.level = function(lev, odf, tg) {

}
