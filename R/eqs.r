
#' Transform a list of pure equilibria into a key-value tables representation.
#'
#' We have one table for each action variable
#'
#' @param eq.li a list of equilibria
#' @param tg the game in table form
#' @param combine if FALSE (default) generate separate tables for each equilibrium in eq.li. Otherwise we combine the tables of each variable over all equilibria.
#' @param add.eq.ind Shall the index of the equilibrium added to the key table? Default TRUE if combine is TRUE, otherwise FALSE.
#' @param reduce.tables (default = TRUE). Shall we try to reduce the rows and columns of the key tables be reduced to get a subset of neccessary keys that perfectly predict the chosen value of an action?
#' @param keep.keys relevant if reduce.tables=TRUE. A character vector of columns that will always be kept as keys and not be reduced. May be helpful when merging or comparing equilibria.
#' @param ignore.keys relevent if reduce.tables=FALSE. A character vector of variables that will always be removed from the key variables, without any check whether they are neccessary or not. By default all parameters of tg are removed, since they are always constant and would only unneccessarily bloat the table.
#' @export
eq.li.tables = function(eq.li, tg,combine=FALSE, add.eq.ind = combine, reduce.tables = TRUE, keep.keys=NULL,ignore.keys = names(tg$params)) {
  restore.point("eq.li.tables")

  res = lapply(seq_along(eq.li), function(eq.ind) {
    eq.tables(eq.li[[eq.ind]],tg=tg, ignore.keys = ignore.keys, reduce.tables=reduce.tables, eq.ind = if (add.eq.ind) eq.ind)
  })
  if (combine) {
    if (length(res)==0) return(NULL)
    if (length(res)==1) return(res[[1]])
    vars = names(res[[1]])
    comb = lapply(vars, function(var) {
      bind_rows(lapply(res, function(li) li[[var]]))
    })
    names(comb) =vars
    return(comb)
  } else {
    return(res)
  }

}



#' Transform a pure equilibrium into a key-value tables representation.
#'
#' We have one table for each action variable
#'
#' @param eq a single equilibrium
#' @param tg the game in table form
#' @param reduce.tables (default = TRUE). Shall we try to reduce the rows and columns of the key tables be reduced to get a subset of neccessary keys that perfectly predict the chosen value of an action?
#' @param keep.keys relevant if reduce.tables=TRUE. A character vector of columns that will always be kept as keys and not be reduced. May be helpful when merging or comparing equilibria.
#' @param ignore.keys relevent if reduce.tables=FALSE. A character vector of variables that will always be removed from the key variables, without any check whether they are neccessary or not. By default all parameters of tg are removed, since they are always constant and would only unneccessarily bloat the table.
#' @param eq.ind An index of the equilibrium that shall be added to the key table. If NULL (default) no column will be added.
#' @export
eq.tables = function(eq, tg,  reduce.tables=TRUE, keep.keys=NULL, ignore.keys = names(tg$params), eq.ind = NULL) {
  restore.point("eq.tables")
  ise.df = tg$ise.df
  stage.df = tg$stage.df
  lev.actions = sapply(tg$action.levels, function(lev.num) tg$lev.li[[lev.num]]$var)
  lev.num = tg$action.levels[1]
  tr = lapply(tg$action.levels, function(lev.num) {
    lev = tg$lev.li[[lev.num]]
    action = lev$var

    oco.rows = which(eq[,action] == 1)
    lev.rows = unique(stage.df[[paste0(".row.", lev.num)]][oco.rows])

    lev.df = lev$lev.df[lev.rows,]
    know.var.groups = unique(lev.df$.know.var.group)

    if (length(know.var.groups)>1) {
      key.df = bind_rows(lapply(know.var.groups, function(.know.var.group) {
        know.vars = lev$know.var.li[[.know.var.group]]
        cols = union(setdiff(know.vars, ignore.keys), action)
        rows = which(lev.df$.know.var.group == .know.var.group)
        table = lev.df[rows,cols]
        if (reduce.tables) table = reduce.key.table(table, keep.keys = keep.keys)
        table

      }))
    } else {
      .know.var.group = know.var.groups
      know.vars = lev$know.var.li[[.know.var.group]]
      cols = union(setdiff(know.vars, ignore.keys), action)
      key.df = lev.df[, cols]
      if (reduce.tables) key.df = reduce.key.table(key.df, keep.keys = keep.keys)

    }
    if (!is.null(eq.ind))
      key.df = cbind(as_data_frame(list(eq.ind=eq.ind)), key.df)
    key.df
  })

  actions = unique(lev.actions)

  if (length(actions)==length(lev.actions)) {
    # No action in multiple levels
    names(tr) = lev.actions
  } else {
    # Some actions have multiple levels
    # aggregate to actions
    tr = lapply(actions, function(action) {
      inds = which(lev.actions==action)
      if (length(inds)==1) return(tr[[inds]])
      return(bind_rows(tr[inds]))
    })
    names(tr)=actions
  }
  return(tr)

}

#' Transform a pure equilibrium into a table-rules representation
#'
#' table-rules are helpful for setting the equilibrium behavior
#' as a fixed rule for a related game, e.g. in order to
#' reduce dimensionality.
eq.table.rules = function(eq, tg, ignore.keys = names(tg$params), add.stage=TRUE, fixed=FALSE, reduce.tables=TRUE) {
  restore.point("pure.eq.to.table.rules")
  ise.df = tg$ise.df
  stage.df = tg$stage.df
  lev.num = tg$action.levels[1]
  rules = lapply(tg$action.levels, function(lev.num) {
    lev = tg$lev.li[[lev.num]]
    action = lev$var

    oco.rows = which(eq[,action] == 1)
    lev.rows = unique(stage.df[[paste0(".row.", lev.num)]][oco.rows])

    lev.df = lev$lev.df[lev.rows,]
    know.var.groups = unique(lev.df$.know.var.group)

    if (length(know.var.groups)>1) {
      tables = lapply(know.var.groups, function(.know.var.group) {
        know.vars = lev$know.var.li[[.know.var.group]]
        cols = union(setdiff(know.vars, ignore.keys), action)
        rows = which(lev.df$.know.var.group == .know.var.group)
        table = lev.df[rows,cols]
        if (reduce.tables) table = reduce.key.table(table)
        table
      })
      rule=list(var=action,fixed=fixed, tables=tables)
      if (add.stage) rule$stage = tg$stages[[lev$stage.num]]$name
    } else {
      .know.var.group = know.var.groups
      know.vars = lev$know.var.li[[.know.var.group]]
      cols = union(setdiff(know.vars, ignore.keys), action)

      table = lev.df[,cols]
      if (reduce.tables) table = reduce.key.table(table)
      rule=list(var=action,fixed=fixed, tables=list(table))
      if (add.stage) rule$stage = tg$stages[[lev$stage.num]]$name
    }
    rule
  })
  rules
}


#' Get equilibrium outcomes from a list of equilibria
#'
#' @param eq.li a list of equilibria
#' @param tg the table form game
#' @param compress if TRUE (default) remove duplicated outcomes from different equilibria
#' @param combine if TRUE (default) combine all outcomes to a single data frame. If FALSE have a list of the different outcomes. If combine=FALSE and compress=TRUE the list only contains unique equilibrium outcomes and may thus have fewer elements than the number of equilibria. Set both combine=FALSE and compress=FALSE to have a list that maps one to one equilibrium outcomes to equilibria.
#' @param cond if not NULL, we compute conditional equilibrium outcomes, see cond.eq.outcome
#' @export
eq.li.outcomes = function(eq.li,  tg=NULL, compress=TRUE, combine=TRUE,cond=NULL, oco.df = tg$oco.df) {
  restore.point("eq.li.outcomes")
  eqo.li = lapply(eq.li, eq.outcome, oco.df=oco.df, tg=tg, cond=cond)
  if (length(eqo.li)>0) {
    is.null = sapply(eqo.li,is.null)
    eqo.li = eqo.li[!is.null]
  }
  if (compress) {
    # unique equilibrium ouctomes
    u.li = unique(eqo.li)
    org.ind = match(eqo.li, u.li)
    eqo.li = lapply(seq_along(u.li), function(i) {
    	restore.point("nsfndfn")
      eqo = u.li[[i]]
      eqo$eq.ind = replicate(NROW(eqo),which(org.ind==i), simplify=FALSE)
      eqo$eqo.ind = i
      eqo
    })
  }
  if (combine) {
    return(xs.col.order(bind_rows(eqo.li),tg))
  }
  return(eqo.li)
}

#' Return the equilibrium outcome of an equilibrium
#'
#' The equilibrium outcome is returned as a data frame. If
#' there are no moves of nature and we have
#' a pure equilbrim it always has a single row.
#'
#' If there are moves of nature or we have a mixed strategy,
#' we get one row for every possible realization of the random
#' variables.
#'
#' You can call eq.expected.outcome to get an expected outcome
#' that will be a single row only.
#'
#' @param eq a single equilibrium
#' @param tg the table form game
#' @param cond if not NULL, we compute conditional equilibrium outcomes, see cond.eq.outcome
#' @export
eq.outcome = function(eq,tg=NULL, cond=NULL, oco.df=tg$oco.df) {
  restore.point("eq.outcome")
  if (!is.null(cond)) return(cond.eq.outcome(eq, cond, oco.df, tg))
  oco.rows = eq[,".prob"] > 0
  eo.df = oco.df[oco.rows,]
  if (NROW(eo.df)==0) return(NULL)

  eo.df$.prob = eq[oco.rows,".prob"]
  xs.col.order(eo.df,tg)
}


#' Get expected equilibrium outcomes from a list of equilibria
#'
#' @param eq.li a list of equilibria
#' @param tg the table form game
#' @param compress if TRUE (default) remove duplicated outcomes from different equilibria
#' @param combine if TRUE (default) combine all outcomes to a single data frame. If FALSE have a list of the different outcomes. If combine=FALSE and compress=TRUE the list only contains unique equilibrium outcomes and may thus have fewer elements than the number of equilibria. Set both combine=FALSE and compress=FALSE to have a list that maps one to one equilibrium outcomes to equilibria.
#' @export
eq.li.expected.outcomes = function(eq.li, tg, compress=TRUE, combine=TRUE) {
  if (!combine) {
    eqo.li = eq.li.outcomes(eq.li, tg=tg, compress=compress, combine=combine)
    res = lapply(eqo.li,expected.outcomes, tg=tg)
  } else {
    eqo.df = eq.li.outcomes(eq.li, tg=tg, compress=compress, combine=combine)
    res = expected.outcomes(eqo.df,tg = tg)
  }
  return(res)
}

#' Get expected equilibrium outcome from a single equilibrium
#'
#' @param eq an equilibrium
#' @param tg the tableform game
eq.expected.outcome = function(eq, tg) {
  eqo.df = eq.outcome(eq, tg)
  expected.outcomes(eqo.df, tg=tg)
}

#' Takes a data frame of equilibrium outcomes and computes
#' expected equilibrium outcomes.
#'
#' @param eqo.df A data frame of equilibrium outcomes as returned by eq.outcome or eq.li.outcomes (with combine=TRUE)
#' @param tg the table-form game
#' @export
expected.outcomes = function(eqo.df=NULL,tg, group.vars=c("eq.ind", "eqo.ind")) {
	restore.point("expected.eq.outcomes")

	if (NROW(eqo.df)==0) return(eqo.df)

	vars = setdiff(colnames(eqo.df),group.vars)
	group.vars = intersect(group.vars, colnames(eqo.df))

	if ("eq.ind" %in% group.vars) {
		if (is.list(eqo.df[["eq.ind"]])) {
			group.vars = setdiff(group.vars, "eq.ind")
			eqo.df = select(eqo.df, - eq.ind)
		}
	}

	#vars = vars[sapply(vars, function(var) is.numeric(eqo.df[[var]]))]

	fun = function(df) {
		restore.point("fun")
		vals = lapply(vars, function(var) {
			if (is.character(df[[var]]) & var != "variant") {
				restore.point("jhsjkhfkdhfh")
				sdf = group_by_(df, "eqo.ind", var) %>%
					s_summarise(paste0('
						.sum.prob = sum(.prob),
						.var.prob = paste0(first(',var,'),ifelse(.sum.prob < 1,paste0("(",round(.sum.prob,2),")"),""))'
					))
				return(paste0(unique(sdf[[".var.prob"]]), collapse=","))
			}

			if (var == ".outcome" | is.character(df[[var]]))
				return(paste0(unique(df[[var]]), collapse=","))
			if (var == ".prob")
				return(sum(df[[var]]))

			if (var=="is.eqo") {
				return(df[[var]][1])
			}

			if (is.numeric(df[[var]]) | is.logical(df[[var]]))
				return(sum(df[[var]] * df$.prob) / sum(df$.prob))

			return(NULL)
		})
		names(vals) = vars
		vals = vals[sapply(vals, function(val) !is.null(val))]
		as_data_frame(c(as.list(df[1,group.vars, drop=FALSE]),vals))
	}


	all.vars = c(group.vars, vars)
	res = eqo.df[,all.vars, drop=FALSE] %>%
		group_by_(.dots=group.vars) %>%
		do(fun(.)) %>%
	  ungroup()
	res

}


#' Return a conditional equilibrium outcome
#'
#' @param eq.li The computed equilibria in gtree form
#' @param cond is a list with variable names and their assumed value
#' we only pick rows from oco.df in which the condition is satisfied
#' we set the probabilities of the conditioned variable values to 1
#' @param expected return expected conditional equilibrium outcomes
#' @param remove.duplicate.eq remove conditional outcomes that are duplicates but arise in different equilibria (who differ off the conditional path)
eq.li.cond.outcomes = function(eq.li, cond, tg=NULL,oco.df=tg$oco.df, expected=FALSE, remove.duplicate.eq=TRUE) {
  restore.point("cond.eq.outcomes")
	li = lapply(seq_along(eq.li), function(i) {
		eq = eq.li[[i]]
		eq.ind = first.non.null(attr(eq,"eq.ind"),i)
		cond.eq.outcome(eq, cond=cond, oco.df=oco.df, tg=tg, eq.ind=eq.ind)
	})
	ceqo = xs.col.order(bind_rows(li),tg)

	# Remove duplicated equilibria that
	# have the same equilibrium outcomes
	if (remove.duplicate.eq) {
    cols = setdiff(colnames(ceqo),c("eq.ind","is.eqo"))
    ceqo = arrange(ceqo, ceqo.ind, !is.eqo)
    dupl = duplicated(ceqo[,cols])
    if (any(dupl))
      ceqo = ceqo[!dupl,,drop=FALSE]
	}

	if (expected)
    return(expected.cond.eq.outcomes(ceqo))


	return(ceqo)
}

#' Expected outcomes from a conditional equilibrium outcome
#'
#' @param ceqo.df The conditional equilibrium outcomes
cond.expected.outcomes = function(ceqo.df) {
  restore.point("expected.cond.eq.outcomes")
  if (!"eqo.ind" %in% colnames(ceqo.df))
    ceqo.df$eqo.ind = ceqo.df$eq.ind
  res = expected.eq.outcomes(ceqo.df, group.vars=c("ceqo.ind","eq.ind"))
  res = select(res,-eqo.ind)
  res
}


#' Return a conditional equilibrium outcome
#'
#' cond is a list with variable names and their assumed value
#' we only pick rows from oco.df in which the condition is satisfied
#' we set the probabilities of the conditioned variable values to 1
cond.eq.outcome = function(eq, cond, tg=NULL, oco.df=tg$oco.df, eq.ind = first.non.null(attr(eq,"eq.ind"),NA), eo.df = eq.outcome(eq=eq, oco.df=oco.df, tg=tg), ceqo.ind=1) {
  restore.point("cond.eq.outcome")
	cond.df = as_data_frame(cond)

	# multiple rows, call function repeatedly
	if (NROW(cond.df)>1) {
		li = lapply(seq_len(NROW(cond.df)), function(row) {
			cond.eq.outcome(eq=eq, cond = cond.df[row,,drop=FALSE], oco.df = oco.df, tg =tg, eq.ind=eq.ind, eo.df = eo.df, ceqo.ind=row+ceqo.ind-1)
		})
		return(bind_rows(li))
	}
  restore.point("cond.eq.outcome.inner")

  cond.vars = names(cond)

  # only consider outcome rows where cond is satisfied
  rows = rep(TRUE,NROW(oco.df))
  for (var in cond.vars) {
    if (length(cond[[var]])==0) next
    rows = rows & (oco.df[[var]] %in% cond[[var]])
  }
  oco.df = oco.df[rows,,drop=FALSE]
  eq = eq[rows,,drop=FALSE]
  # set the probabilities of the variables, we condition on to 1
  eq[,intersect(cond.vars,colnames(eq))]=1
  # compute conditional outcome probabilities
  eq[,".prob"] = rowProds(eq[,-NCOL(eq),drop=FALSE])

  oco.rows = eq[,".prob"] > 0
  ceo.df = oco.df[oco.rows,]
  ceo.df$.prob = eq[oco.rows,".prob"]
	ceo.df$eq.ind = eq.ind

	# find the conditional outcomes that are equilibrium outcomes
	keys = setdiff(
		intersect(colnames(ceo.df), colnames(eo.df)),
		c(".prob",".outcome","eq.ind","eqo.ind")
	)
	eo.df$is.eqo = TRUE
	ceo.df = left_join(ceo.df, eo.df[,c(keys,"is.eqo")],by=keys)
	ceo.df$ceqo.ind = ceqo.ind
	ceo.df$is.eqo[is.na(ceo.df$is.eqo)] = FALSE

  xs.col.order(ceo.df,tg)
}

#' Helper function to reduce the key columns of
#' a key-value table
#'
#' @param table the key value table
#' @param var the column name that holds the value.
#'        By default the last column.
#' @param keep.keys a character vector of key columns that
#'        shall never be removed.
#' @export
reduce.key.table = function(table, var=colnames(table)[NCOL(table)], keep.keys=NULL) {
  if (NROW(table)<=1) return(table)

  # All variables have the same number
  if (n_distinct(table[[var]])==1) {
    return(table[1,var])
  }

  keys = setdiff(colnames(table), var)
  if (length(keys)<=1) return(table)


  keep.keys = intersect(keep.keys, keys)

  if (length(keep.keys)>0) {
    remaining.keys = setdiff(keys, keep.keys)
    if (length(remaining.keys)==0)
      return(table)

    if (is.multi.perfect.predictor(keep.keys,var, table))
      return( unique(table[,c(keep.keys,var)]) )


    perf.pred = remaining.keys[sapply(remaining.keys, function(key) {
      is.multi.perfect.predictor(c(key, remaining.keys),var,table)
    })]

    if (length(perf.pred)==0) return(table)

    return( unique(table[,c(c(keep.keys, perf.pred[1]),var)]) )
  }

  perf.pred = keys[sapply(keys, function(key) {
    is.perfect.predictor(table[[key]],table[[var]])
  })]

  # Don't simplify beyond perfect predictors
  if (length(perf.pred)==0) return(table)

  # Select perfect predictor with minimum number
  # of elements
  if (length(perf.pred)>1) {
    len = sapply(perf.pred, function(key) {
      n_distinc(table[[key]])
    })
    perf.pred = perf.pred[which.min(len)]
  }
  rows = !duplicated(table[[perf.pred]])
  return(table[rows,c(perf.pred, var)])
}


# Is x a perfect predictor for y
# Every value of x must have the same value y
is.perfect.predictor = function(x,y, df = as_data_frame(list(x=x,y=y))) {
  dupl = duplicated(df)
  nx = df[[1]][!dupl]
  n_distinct(nx) == length(nx)
}

# Is x a perfect predictor for y
# Every value of x must have the same value y
is.multi.perfect.predictor = function(xcols,ycol,df, sep="|°") {
  x = paste0(df[,xcols], sep=sep)
  is.perfect.predictor(x=x, y=df[[y]])
}


get.eq.id = function(tg.id=tg$tg.id, just.spe=TRUE, mixed=FALSE, tg=NULL, solvemode=NULL) {
 	eq.id = paste0(tg$tg.id)
 	if (!is.null(solvemode)) {
 		return(paste0(eq.id,"__",solvemode))
 	}
 	if (just.spe)
 		eq.id = paste0(eq.id,"_spe")
 	if (mixed)
 		eq.id = paste0(eq.id,"_mixed")
 	eq.id

}
