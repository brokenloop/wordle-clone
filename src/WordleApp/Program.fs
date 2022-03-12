open System
open Newtonsoft.Json

[<Literal>]
let STATE_FILE = "startingState.json"
let WORD_LENGTH = 5
let MAX_GUESSES = 6

type Result = 
    | Win
    | Lose

type TurnContext = 
    {
        NumGuesses : int
        PrevGuesses : List<string>
    }

type GameState = 
    | InProgress of TurnContext
    | Finished of Result 

type Context = 
    {
        ValidWords : Set<string>
        TargetWord : string
        GameState : GameState 
    }

type Action = 
    | Guess of string
    | WrongLength
    | InvalidWord

// * -> ch not in target
// lower -> ch in target, not right location
// upper -> ch in target, right location
let matchingLetters (target: string) (guess: string) =   
    let mutable res = ""
    for i in 0..(guess.Length-1) do
        let ch = guess.[i]
        if ch = target.[i] then
            res <- res + System.Char.ToUpper(ch).ToString()
        elif target.Contains(ch) then
            res <- res + ch.ToString()
        else
            res <- res + "*"
    res

let randomElement seq = 
    let r = Random()
    seq 
    |> Seq.sortBy (fun _ -> r.Next())
    |> Seq.head

let update targetWord action (turn: TurnContext) = 
    match action with 
        | Guess guess ->
            if guess = targetWord then
                printfn "correct! you win" 
                Finished Win
            elif turn.NumGuesses = MAX_GUESSES then
                printfn "you lose!"
                Finished Lose
            else 
                let newMatch = matchingLetters targetWord guess
                let newTurn = {
                    NumGuesses = turn.NumGuesses + 1;
                    PrevGuesses = newMatch :: turn.PrevGuesses
                    }

                for prevGuess in Seq.rev newTurn.PrevGuesses do
                    printfn "%s" prevGuess
                InProgress newTurn
        | WrongLength ->
            printfn "wrong length"
            InProgress turn
        | InvalidWord ->
            printfn "invalid word"
            InProgress turn
            
let getAction context =
    let guess = Console.ReadLine()
    if (guess.Length <> WORD_LENGTH) then
        WrongLength
    elif not (Set.contains guess context.ValidWords) then
        InvalidWord
    else 
        Guess guess

let rec gameloop context =
    match context.GameState with
        | InProgress turnContext -> 
            let action = getAction context
            let newState = update context.TargetWord action turnContext
            gameloop {context with GameState = newState}
        | Finished _ -> 
            context
        
[<EntryPoint>]
let main args =
    let rawJson = System.IO.File.ReadAllText STATE_FILE
    let mutable startingContext = JsonConvert.DeserializeObject<Context> rawJson
    startingContext <- {startingContext with 
                            TargetWord = randomElement startingContext.ValidWords
                            GameState = InProgress {NumGuesses = 0; PrevGuesses = []}}

    printfn "How to play:"
    printfn "You have 6 guesses to find the correct 5 letter word"
    printfn "*         => letter not in word"
    printfn "lowercase => letter in word, but wrong position"
    printfn "UPPERCASE => letter in word, in right position"
    printfn ""

    gameloop startingContext |> ignore
    0