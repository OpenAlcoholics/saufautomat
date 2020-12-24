module Model

type CardsVersion =
    | I18N
    | V2

type Settings =
    { MinimumSips: int
      MaximumSips: int
      Remote: bool
      Audio: bool
      Language: string
      CardsVersion: CardsVersion }

type RoundInformation =
    { CardsToPlay: int
      InitialPlayerIndex: int }

type Model =
    { Players: Player.Type list
      ActiveCards: (Card.Type * Player.Type option) list
      CurrentCard: Card.Type option
      Cards: Card.Type list
      CurrentPlayer: Player.Type option
      Counter: int
      DisplayPlayerNameDuplicateError: bool
      InitialLoad: bool
      Settings: Settings
      Round: int
      RoundInformation: RoundInformation }

type Msg =
    | InitialLoad
    | ChangeActiveCard
    | ChangeActivePlayer
    | IncrementCounter
    | AddActiveCard of Card.Type * Player.Type option
    | AddCards of Card.RawType list
    | AddPlayer of Player.Type
    | RemovePlayer of Player.Type
    | TogglePlayerActivity of Player.Type
    | DisplayPlayerNameDuplicate
    | HidePlayerNameDuplicate
    | DecrementActiveRoundCards
    | DecrementPlayerRoundCards
    | UseActiveCard of Card.Type * Player.Type
    | SaveSettings
    | ChangeAudioSetting
    | ChangeRemoteSetting
    | Reset
    | AdvanceTurn
    | AdvanceRound
    | ChangeLanguage of string
    | ChangeCardsVersion of CardsVersion
    | PlayAudio
    | RemoveActiveCard of Card.Type
    | RemoveCardFromSession of Card.Type
    | AddNoteToActiveCard of Card.Type * Player.Type option
    | ReassignCard of Card.Type
    | NoopMsg