module Card

type RawType =
    { id: int
      text: string
      count: int
      uses: int
      rounds: int
      personal: bool
      remote: bool
      unique: bool }

[<CustomEquality; NoComparison>]
type Type =
    { Id: int
      Text: string
      Count: int
      Uses: int
      Rounds: int
      Personal: bool
      Remote: bool
      Unique: bool
      Note: string option
      StartingRound: int option
      ReplacedText: string
      OriginalCount: int }
    override this.Equals(other) =
        match other with
        | :? Type as other -> this.Id = other.Id
        | _ -> false

    override this.GetHashCode() = hash this.Id

let Into raw =
    { Id = raw.id
      Text = raw.text
      Count = raw.count
      Uses = raw.uses
      Rounds = raw.rounds
      Personal = raw.personal
      Remote = raw.remote
      Unique = raw.unique
      Note = None
      StartingRound = None
      ReplacedText = raw.text
      OriginalCount = raw.count }

let decreaseCount card cards =
    match card with
    | Some card -> List.map (fun c -> if c = card then { c with Count = c.Count - 1 } else c) cards
    | None -> cards

let getDistinctCount cards =
    (List.map (fun c -> c.Id) cards |> List.distinct)
        .Length

let Default =
    { Id = -1
      Text = ""
      Count = 0
      Uses = 0
      Rounds = 0
      Personal = false
      Remote = false
      Unique = true
      Note = None
      StartingRound = None
      ReplacedText = ""
      OriginalCount = 0 }
