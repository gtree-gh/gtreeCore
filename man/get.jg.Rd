% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_game.r
\name{get.jg}
\alias{get.jg}
\title{Get a game in jg format by its gameId}
\usage{
get.jg(gameId, json.file = paste0(game.dir, "/", gameId, ".json"),
  game.dir = file.path(games.dir, gameId),
  games.dir = get.games.dir(project.dir), project.dir = get.project.dir(),
  jg = NULL)
}
\description{
Looks in the current project directory by default
and parses the json file
}
