example.new.vg = function() {
  setwd("D:/libraries/gtree/myproject")

  vg = new.vg(
    gameId = "UG",
    params = list(numPlayers=2, cake=4),
    stages = list(
      stage("ProposerStage",
        player=1,
        actions = list(
          action("offer",~0:cake)
        )
      ),
      stage("ResponderStage",
        player=2,
        observe="offer",
        actions = list(
          action("accept",c(FALSE,TRUE))
        )
      ),
      stage("PayoffStage",
        player=1:2,
        observe=c("offer","accept"),
        compute=list(
          payoff_1 ~ (cake-offer)*accept,
          payoff_2 ~ offer*accept
        )
      )
    )
  )
  tg = vg.to.tg(vg)
  tg

  vg$stages
  setwd("D:/libraries/gtree/myproject")

  vg = new.vg(
    gameId = "RandomCostCournot",
    params = list(numPlayers=2, a=10, qMax=5,
      c2=0, c1Low=0, c1High=2),
    stages = list(
      stage("drawCostStage",
        nature = list(
          natureMove("c1",~c(c1Low,c1High))
        )
      ),
      stage("q1Stage",
        player=1,
        observe="c1",
        actions = list(
          action("q1",~0:qMax)
        )
      ),
      stage("q2Stage",
        player=2,
        actions = list(
          action("q2",~0:qMax)
        )
      ),
      stage("PayoffStage",
        player=1:2,
        compute=list(
          Q ~ q1+q2,
          P ~ a-Q,
          payoff_1 ~ (P-c1)*q1,
          payoff_2 ~ (P-c2)*q2
        )
      )
    )
  )
  vg
  unique(tg$oco.df$c1)
  tg = vg.to.tg(vg,add.sg = TRUE)
  tg

  # Internal solver
  eq.li = gtree.solve.spe(tg=tg)
  # Gambit
  eq.li = gambit.solve.eq(tg=tg, save.eq=FALSE)


  eqo.li = expected.eq.outcomes(eq.li=eq.li, tg=tg)
  eqo.li

  pure.eq.to.tables(eq.li[[1]], tg=tg)
  pure.eq.to.table.rules(eq.li[[1]], tg=tg)

}

#' Create a new game in stage form
new.game = new.vg = function(gameId, params, stages, variant="") {
  restore.point("new.game")
  vg = as.environment(list(
    gameId = gameId,
    variant = variant,
    params = params,
    stages = stages
  ))
  class(vg) = c("gtree_vg","environment")
  vg
}


f2c = function(x) {
  if (is(x,"formula")) return(x[[length(x)]])
  x
}

action = function(name, set, ...) {
  list(name=name,set=f2c(set),...)
}

natureMove = function(name, set, probs=NULL,...) {
  list(name=name,set=f2c(set),probs=f2c(probs),...)
}

name.by.name = function(li) {
  names = sapply(li, function(x) x$name)
  names(li) = names
  li
}

stage = function(name, player=NULL, condition=NULL, observe=NULL, compute=NULL, nature=NULL, actions=NULL,...) {
  restore.point("stage")

  player = f2c(player)
  condition = f2c(condition)
  observe=f2c(observe)
  compute = name.by.name(lapply(seq_along(compute), function(i) {
    trans = compute[[i]]
    restore.point("stage385")
    if (is(trans,"formula")) {
      if (length(trans)==3)
        return(list(name=as.character(trans[[2]]),formula=trans[[3]]))
      return(list(name = names(compute)[[i]], formula=trans[[2]]))
    }
    if (is.null(trans$name)) trans$name = names(compute)[[i]]
    trans$formula = f2c(trans$formula)
    trans
  }))
  nature = name.by.name(lapply(seq_along(nature), function(i) {
    x = nature[[i]]
    if (is.null(x$name)) x$name = names(nature)[[i]]
    x$set = f2c(x$set)
    x$prob = f2c(x$prob)
    x
  }))
  actions = name.by.name(lapply(seq_along(actions), function(i) {
    x = actions[[i]]
    if (is.null(x$name)) x$name = names(actions)[[i]]
    x$set = f2c(x$set)
    x
  }))
  nlist(name,player,condition,observe, compute,nature, actions,...)
}

memory.list = function(x) {
  if (is.environment(x))
    x = as.list(x)

  x.size = format(object.size(x), units="auto")
  if (!is.list(x))
    return(x.size)

  if (length(x)==0) return("0")

  restore.point("fhdfjdhfk")

  # Sort by size
  sizes = lapply(x, object.size)
  ord = order(-unlist(sizes))
  x = x[ord]

  fsizes = lapply(sizes, format, units="auto")
  kids = lapply(x,memory.list)
  names(kids) = paste0(names(x),": ", fsizes)

  size.li = list(x.size)
  names(size.li) = x.size
  c(size.li,kids)
}
