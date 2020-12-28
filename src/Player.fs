module Player

open System

[<CustomEquality; NoComparison>]
type Type =
    { GId: string
      Name: string
      Active: bool
      CardsPlayed: int }

    override this.Equals(other) =
        match other with
        | :? Type as otherPlayer -> this.Name = otherPlayer.Name
        | _ -> false

    override this.GetHashCode() = hash this.Name

let create name =
    { GId = Guid.NewGuid().ToString()
      Name = name
      Active = true
      CardsPlayed = 0 }

let compareOption (player1: Type option) (player2: Type option): bool =
    if player1.IsSome && player2.IsSome then player1.Value = player2.Value else false

let filterActiveCards player cards =
    List.map (fun (c, _) -> c) (List.filter (fun (_, p) -> compareOption (Some player) p) cards)

let getActive players: Type list =
    List.filter (fun player -> player.Active) players

let getIndex player players =
    match player with
    | Some p -> (List.tryFindIndex ((=) p) players)
    | None -> None

let isCurrent player current = compareOption (Some player) current
