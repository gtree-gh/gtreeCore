# We take a tg game and can fix some actions
#
# We transform these actions into moves of nature
# with specified move probabilities that can depend
# on specific values of previous variables

# Applications:
#
# 1. Find best-reply against data: Fix moves
#    of all but one player to the probabilities
#    coming from a data set.
#
# 2. Check for best replies. Fix some strategies
#    to their proposed equilibrium values.
#    Now let us find best replies against them
#    Can be used to test equilibrium computations.
#
# 3. Compute level-k equilibria
#
# 4. Part of analyzing norm equilibria in a game.


example.fix.actions = function() {
  setwd("D:/libraries/gtree/myproject")
	gameId = "UltimatumGame"
	tg = get.tg(gameId = gameId, never.load = FALSE)

	oco.df = tg$oco.df
  # Only accept offers of at least 5

	fix.df = data_frame(offer=0:7, accept=(offer<=5)*1)
  var = get.fix.df.var(fix.df)

  lev = tg$lev.li[[2]]
  names(lev)

  res = lev.action.to.nature(lev, fix.df,var=var)


  nlev = res[[2]]
}

fix.tg.actions = function(tg, fix.df, var=NULL, fix.li=NULL, omit.zero.prob=TRUE) {
  restore.point("fix.tg.actions")
  new.levs = lapply(tg$lev.li, function(lev) {
    lev.action.to.nature(lev, fix.df=fix.df, var=var, fix.li=fix.li, omit.zero.prob = omit.zero.prob)
  })

  new.levs

}


lev.action.to.nature = function(lev, fix.df,var = NULL, fix.li=NULL,omit.zero.prob=TRUE,...) {
  restore.point("lev.action.to.nature")

  if (is.null(var))
    var = get.fix.df.var(fix.df)

  if (!is.null(fix.li)) {
    res = lapply(fix.li, function(fix.df) {
      lev.action.to.nature(lev, fix.df, var=var, omit.zero.prob = omit.zero.prob,...)
    })
    return(do.call(c, res))
  }

  # Only actions can be fixed
  if (lev$type != "action")
    return(list(lev))

  # A different action than the one fixed
  if (var!=lev$var)
    return(list(lev))

  lev.df = lev$lev.df
  cols = !str.starts.with(colnames(lev.df),".row.")
  lev.df = lev.df[,cols]
  lev.df$.ORG.ROW = seq_len(NROW(lev.df))

  by.cols = intersect(colnames(lev.df), colnames(fix.df))
  key.cols = setdiff(by.cols, var)

  # If fix.df assigns deterministic actions
  # specify it as a move of nature with probability 1
  fix.has.prob = ".move.prob" %in% fix.df
  if (!fix.has.prob) {
    fix.df$.move.prob = 1
    if (!omit.zero.prob) {
      # Only use cases specified by keys
      left = semi_join(lev.df[,by.cols], fix.df, by=key.cols)
      # Match to all values of var
      fix.df = left_join(left, fix.df[,c(by.cols,".move.prob")], by=by.cols)
      # Set probability to 0 for non-played values of var
      fix.df$.prob[is.na(fix.df$.move.prob)] = 0
    }
  }


  # Create data frame for move of nature
  join.cols = setdiff(colnames(lev.df), c(".info.set.ind",".info.set.move.ind", ".info.set",".move.ind",".move.prob"))
  join.cols = join.cols[!str.starts.with(join.cols,".row.")]
  nat.df = fix.df %>% left_join(lev.df[, join.cols], by=by.cols)

  # Order columns in original order
  cols = intersect(colnames(lev.df), colnames(nat.df)) %>%
      setdiff(".prob") %>% c(".move.prob", ".prob")
  nat.df = nat.df[,cols]
  nat.df$.prob = nat.df$.prob * nat.df$.move.prob

  # Select original rows from know.li
  # This means the knowledge structure
  # is assumed not to change.
  #
  # The player who has chosen an action before
  # will also know the outcome of the move of nature
  #
  # That is for example neccessary when looking
  # at norm equilibria
  nat.know.li = lev$know.li
  if (NROW(nat.df) < NROW(lev.df)) {
    nat.know.li = lapply(nat.know.li, function(know.mat) {
      know.mat[nat.df$.ORG.ROW,,drop=FALSE]
    })
  }

  nat.lev = nlist(
    type="nature",
    var = var,
    lev.num = lev$lev.num,
    stage.num = lev$stage.num,
    player=0,
    lev.df=nat.df,
    know.li = lev$know.li # TO DO: HOW TO ADAPT know.li
  )


  # Check if some actions remain
  act.df = anti_join(lev.df, fix.df, by=key.cols)

  # All nodes where transformed to a move of nature
  if (NROW(act.df)==0) {
    return(list(nat.lev))
  }

  # Some nodes remain actions
  act.know.li = lapply(lev$know.li, function(know.mat) {
    know.mat[act.df$.ORG.ROW,,drop=FALSE]
  })
  act.lev = lev
  act.lev$lev.df = act.df
  act.lev$know.li = act.know.li

  # Return both new nature level
  # and an level for remaining action nodes
  return(list(act.lev, nat.lev))
}

get.fix.df.var = function(fix.df) {
  var = attr(fix.df,"fix.var")
  if (!is.null(var)) return(var)

  cols = setdiff(colnames(fix.df),".prob")
  return(cols[length(cols)])
}
