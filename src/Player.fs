module Player

[<CustomEquality; NoComparison>]
type Type =
    { Name: string
      Active: bool
      CardsPlayed: int }

    override this.Equals(other) =
        match other with
        | :? Type as otherPlayer ->
            this.Name = otherPlayer.Name
        | _ -> false

    override this.GetHashCode() = hash this.Name

let create name =
    { Name = name
      Active = true
      CardsPlayed = 0 }

let compareOption (player1: Type option) (player2: Type option) : bool =
    if player1.IsSome && player2.IsSome then player1.Value.Name = player2.Value.Name else false

let filterActiveCards player cards =
    List.map (fun (c, _) -> c) (List.filter (fun (_, p) -> compareOption (Some player) p) cards)
