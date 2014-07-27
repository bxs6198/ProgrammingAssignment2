Guess <- function(max) {
    secret.number <- sample(1:max, 1)
    total.guesses <- 1
    finished <- FALSE
    
    message(sprintf("I'm thinking of a number from 1 to %d.", max))
    
    self.list <- list(
        get.info = function() {
            message(
                sprintf(
                    "You %s guessed the number!", 
                    if (finished) "have already"
                    else "still haven't"
                )
            )
            message(sprintf("Guess number: %d", total.guesses))
            message(sprintf("The number is taken from 1 to %d.", max))
        },
        get.clues = function(guess) {
            if (finished) message("You've already guessed my number!")
            else {
                if (guess == secret.number) {
                    message("Congratulations!")
                    message(sprintf("You got it in %d guesses!", total.guesses))
                    finished <<- TRUE
                } else {
                    if (secret.number < guess) message("Too high!")
                    else message("Too low!")
                    total.guesses <<- total.guesses + 1
                }
            }
        }
    )
    
    class(self.list) <- "Guess"
    self.list
}