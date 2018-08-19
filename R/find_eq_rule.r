# Try to transform an equilibrium into a simple rule
#
# Almost nothing implemented yet.
# Not clear that we get large value from this route


example.table.rule = function() {
  setwd("D:/libraries/gtree/myproject")

  gameId = "UltimatumGame"
	tg = get.tg(gameId = gameId,never.load = FALSE)
  ise.df = tg$ise.df

	eq.li = gambit.solve.eq(tg)
  eq = eq.li[[1]]
  pure.eq.to.tables(eq, tg)
  pure.eq.to.table.rules(eq, tg)

  pure.eq.li.to.table.rule(eq.li, tg)
}

#' Transform a list of pure equilibria into a table-rule representation
pure.eq.li.to.tables = function(eq.li, tg, ignore.keys = names(tg$params)) {
  lapply(eq.li, pure.eq.to.table.rule, tg=tg, ignore.keys = ignore.keys, as.rules=as.rules)
}



#' Transform a pure equilibrium into an action-keys-table representation
pure.eq.to.tables = function(eq, tg, ignore.keys = names(tg$params)) {
  restore.point("pure.eq.to.tables")
  ise.df = tg$ise.df
  stage.df = tg$stage.df
  lev.actions = sapply(tg$action.levels, function(lev.num) tg$lev.li[[lev.num]]$var)
  lev.num = 1
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
        lev.df[rows, cols]
      }))
    } else {
      .know.var.group = know.var.groups
      know.vars = lev$know.var.li[[.know.var.group]]
      cols = union(setdiff(know.vars, ignore.keys), action)
      key.df = lev.df[, cols]
    }
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
pure.eq.to.table.rules = function(eq, tg, ignore.keys = names(tg$params), add.stage=TRUE) {
  restore.point("pure.eq.to.table.rules")
  ise.df = tg$ise.df
  stage.df = tg$stage.df
  lev.num = 1
  li = lapply(tg$action.levels, function(lev.num) {
    lev = tg$lev.li[[lev.num]]
    action = lev$var

    oco.rows = which(eq[,action] == 1)
    lev.rows = unique(stage.df[[paste0(".row.", lev.num)]][oco.rows])

    lev.df = lev$lev.df[lev.rows,]
    know.var.groups = unique(lev.df$.know.var.group)

    if (length(know.var.groups)>1) {
      rules = lapply(know.var.groups, function(.know.var.group) {
        know.vars = lev$know.var.li[[.know.var.group]]
        cols = union(setdiff(know.vars, ignore.keys), action)
        rows = which(lev.df$.know.var.group == .know.var.group)
        rule=list(var=action, table=lev.df[rows,cols])
        if (add.stage) rule$stage = tg$stages[[lev$stage.num]]$name
        rule
      })
    } else {
      .know.var.group = know.var.groups
      know.vars = lev$know.var.li[[.know.var.group]]
      cols = union(setdiff(know.vars, ignore.keys), action)
      rule=list(var=action, table=lev.df[,cols])
      if (add.stage) rule$stage = tg$stages[[lev$stage.num]]$name
      rules = list(rule)
    }
    rules
  })
  do.call(c, li)
}


# Specify the key variables for each action that can be played in a tg
make.tg.action.keys = function(tg) {
  actions = unique(tg$ise.df$.var)

}

example.find.perfect.predictor.cols = function() {
  T = 100
  dat = data_frame(a=sample(0:1, T, replace = TRUE), b=runif(T,-1,1), c=b^2, y=(1-c)>0.5)
  df = select(df,b,y)
  is.perfect.predictor(df = select(dat,b,y))
  is.perfect.predictor(df = select(dat,c,y))

  is.monotone.predictor(df = select(dat,b,y))
  is.monotone.predictor(df = select(dat,c,y))

  accept = quote((payoff_2 - alpha*(payoff_1-payoff_2))>0)

}

find.perfect.predictor.cols = function(df, var) {
  restore.point("find.perfect.predictor.cols")

}

# Is x a perfect predictor for y
# Every value of x must have the same value y
is.perfect.predictor = function(x,y, df = as_data_frame(list(x=x,y=y))) {
  dupl = duplicated(df)
  nx = df[[1]][!dupl]
  n_distinct(nx) == length(nx)
}

# Is a numeric x a monotone predictor for y
is.monotone.predictor = function(x,y, df = as_data_frame(list(x=x,y=y))) {
  ord = order(df[[1]])
  df = df[ord,]
  is.highest = which(!is.true(lead(df[[2]]) == df[[2]]))
  n_distinct(df[[2]][is.highest]) == length(is.highest)
}
