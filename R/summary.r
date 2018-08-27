print.gtree_tg = function(tg) {
  no.oco = format(NROW(tg$oco.df), big.mark=" ")
	no.ise = format(NROW(tg$ise.df), big.mark=" ")
	num.moves = format(sum(tg$ise.df$.num.moves), big.mark=" ")
	num.all.sp = format(prod(tg$ise.df$.num.moves), big.mark=" ")
	avg.moves = format(round(mean(tg$ise.df$.num.moves),1))

	num.strat = sapply(1:tg$numPlayers, function(i) {
	  prod(tg$ise.df$.num.moves[tg$ise.df$.player==i])
	})

	num.ise = sapply(1:tg$numPlayers, function(i) {
	  n_distinct(tg$ise.df$.info.set[tg$ise.df$.player==i])
	})


  cat(paste0("\nTableform game: ", tg$tg.id, " (ca. ", format(object.size(as.list(tg))-object.size(tg$stage.df), units="auto"),")\n"))
  cat(paste0("\n  - ",no.oco, " possible outcomes"))
  cat(paste0("\n  - ", no.ise, " information sets (", paste0(num.ise,collapse=" + "),")"))
  cat(paste0("\n  - " ,num.all.sp, " normal-form strategy profiles (",paste0(num.strat,collapse=" * "),")"))

  if (is.null(tg$sg.df)) {
    cat(paste0("\n\n  -- Subgames not yet computed ---"))
  } else {
		no.sg  = format(NROW(tg$sg.df), big.mark=" ")
		no.all.sp = format(tg$sg.df$.num.strats[1],big.mark = " ", scientific = 9)
		no.sp = format(sum(tg$sg.df$.num.strats.without.desc), big.mark=" ",scientific = 9)

		cat(paste0("\n  - ",no.sg, " subgames"))
		cat(paste0("\n  - ",no.sp, " relevant subgame strategy profiles"))
  }
  cat("\n")
}

print.gtree_vg = function(vg) {
  cat(paste0("\nStage-form game (vg): ", vg$gameId, if (!is.empty(vg$variant)) paste0("_",vg$variant)))

  cat(paste0("\n\nParameters: ", paste0(names(vg$params),"=",vg$params, collapse=", ")))

  for (stage in vg$stages) {
    cat("\n")
    print.gtree_stage(stage)
  }
  cat("\n")

}

print.gtree_stage = function(stage) {
  #cat(paste0("\nStage: ", stage$name))
  cat(paste0("\n", stage$name))

  if (!is.empty(stage$player))
    cat(paste0("\n  Player: ", form2string(stage$player)))

  if (!is.empty(stage$condition))
    cat(paste0("\n  Condition: ", form2string(stage$condition)))

  if (!is.empty(stage$observe))
    cat(paste0("\n  Observe: ", form2string(stage$observe)))

  for (x in stage$compute)
    cat(paste0("\n  Compute ", x$name, if (!is.null(x$tables)) " specified by tables." else paste0(" = ", form2string(x$formula))))
  for (x in stage$nature)
    cat(paste0("\n  Nature ", x$name, " in ", form2string(x$set)), ", Prob = ",form2string(x$probs))
  for (x in stage$actions)
    cat(paste0("\n  Action ", x$name, " in ", form2string(x$set)))


}

form2string = function(x) {
  if (is.call(x)) {
    return(capture.output(print(x)))
  }
  paste0(as.character(x), collapse=", ")
}

