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
      Unique: bool }
    override this.Equals(other) =
        match other with
        | :? Type as other ->
            this.Id = other.Id
        | _ -> false

    override this.GetHashCode() = hash this.Id

let Into raw =
        {
            Id = raw.id
            Text = raw.text
            Count = raw.count
            Uses = raw.uses
            Rounds = raw.rounds
            Personal = raw.personal
            Remote = raw.remote
            Unique = raw.unique
        }