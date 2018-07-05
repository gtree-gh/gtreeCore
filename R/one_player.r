# Internal algorithm to solve tg games in which just one player makes a decision
#
# Can be used to quickly...
# 1. ...find best replies
# 2. ...solve for first best solution

example.solve.single.player.tg = function() {
	gameId = "UG3"
	tg.org = get.tg(gameId = gameId, never.load = !FALSE)
	eq.li = get.eq(tg = tg.org, never.load=TRUE)
  eq = eq.li[[1]]

  tg = make.best.reply.tg(tg.org, eq, player=2, tremble.prob = 1/100)

  solve.single.player.tg(tg)
}

solve.single.player.tg = function(tg) {
  restore.point("solve.single.player.tg")

  util.cols = c(paste0("util_",1:tg$numPlayers))

  # Outcomes that will be reduced via backward induction
  odf = tg$oco.df

  # Back
  lev.num = length(tg$lev.li)+1

  while(lev.num > 1) {
    lev.num = lev.num-1
    lev = tg$lev.li[[lev.num]]
    lev.df = lev$lev.df

    join.cols = intersect(tg$lev.vars, colnames(lev.df))
    if (lev$type=="nature") {
      # Outcomes that are not reached at this level
      # add them later again to odf
      omitted.odf = anti_join(odf, lev.df, by=join.cols)

      # Join utilities to lev.df
      olev.df = inner_join(lev.df, odf[,c(join.cols, util.cols)],
        by=join.cols)

      # Compute expected utilities
      # by grouping over .node.ind
      odf  = olev.df %>%
        group_by_at(c(setdiff(join.cols, lev$var),".node.ind")) %>%
        summarize_at(util.cols, funs(weighted.mean(., .move.prob))) %>%
        ungroup() %>%
        select(-.node.ind)

      # rbind initially omitted rows to the new odf
      if (NROW(omitted.odf)>0) {
        odf = bind_rows(odf, omitted.odf)
      }


    } else if (lev$type=="action") {


    }
  }

}

single.player.reduce.nature.level = function(lev, odf, tg) {

}
