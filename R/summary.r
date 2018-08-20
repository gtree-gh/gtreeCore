print.gtree_tg = function(tg) {
  no.oco = format(NROW(tg$oco.df), big.mark=" ")
	no.ise = format(NROW(tg$ise.df), big.mark=" ")
	num.moves = format(sum(tg$ise.df$.num.moves), big.mark=" ")
	num.all.sp = format(prod(tg$ise.df$.num.moves), big.mark=" ")
	avg.moves = format(round(mean(tg$ise.df$.num.moves),1))

	num.strat = sapply(1:tg$numPlayers, function(i) {
	  prod(tg$ise.df$.num.moves[tg$ise.df$.player==i])
	})

  cat(paste0("\nTableform game: ", tg$tg.id, " (", format(object.size(as.list(tg)), units="auto"),")\n"))
  cat(paste0("\n  - ",no.ise, " information sets (", tg$numPlayers, " players)"))
  cat(paste0("\n  - ",paste0(num.strat,collapse=" * ")," = " ,num.all.sp, " strategy profiles (normal form)"))
  cat(paste0("\n  - ",no.oco, " possible outcomes"))

  if (is.null(tg$sg.df)) {
    cat(paste0("\n\n  -- Subgames not yet computed ---"))
  } else {
		no.sg  = format(NROW(tg$sg.df), big.mark=" ")
		no.all.sp = format(tg$sg.df$.num.strats[1],big.mark = " ", scientific = 9)
		no.sp = format(sum(tg$sg.df$.num.strats.without.desc), big.mark=" ",scientific = 9)

		cat(paste0("\n  - ",no.sg, " subgames"))
		cat(paste0("\n  - ",no.all.sp, " strategy profiles (normal-form)"))
		cat(paste0("\n  - ",no.sp, " strategy profiles (backward induction)"))
  }
}
