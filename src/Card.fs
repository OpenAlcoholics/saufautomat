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