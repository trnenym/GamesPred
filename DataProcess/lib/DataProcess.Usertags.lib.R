# Frequent hand-picked user tags
tags.relevant <- c("2D", "4X", "BoardGame", "CardGame", "CityBuilder", "Crafting", "Cyberpunk", "DatingSim", "Episodic", "FamilyFriendly", "Fantasy"
                   , "FemaleProtagonist", "Fighting", "FirstPerson", "Flight", "FPS", "HiddenObject", "Horror", "JRPG"
                   , "Medieval", "Noir", "Nudity", "OpenWorld", "Parkour", "PixelGraphics", "Platformer", "PointClick", "Puzzle", "Remake", "Retro"
                   , "Rhythm", "Roguelike", "Roguelite", "RTS", "Sandbox", "Scifi", "Space", "Stealth", "Steampunk", "StoryRich"
                   , "Superhero", "Survival", "SurvivalHorror", "ThirdPerson", "ThirdPersonShooter", "TowerDefense", "TurnBased"
                   , "TurnBasedStrategy", "VisualNovel", "WalkingSimulator", "WorldWarII", "Zombies")

# Returns the above list with an optional prefix
get.usertags.names <- function(prefix = "") {
  return(unname(sapply(tags.relevant, function(x) paste(prefix, x, sep = ""))))
}

# Returns a logical vector indicating whether each tag is present in the supplied string
get.usertags <- function(usertags) {
  usertags <- gsub('amp;','', usertags)
  usertags <- gsub(' ','', usertags)
  usertags <- gsub('-','', usertags)
  usertags <- gsub('&','', usertags)
  usertags <- gsub('\'','', usertags)

  tags.list <- unname(sapply(usertags, function(x) strsplit(x, ";")))[[1]]

  result <- unname(sapply(tolower(tags.relevant), function(x) x %in% tolower(tags.list)))

  return(result)
}