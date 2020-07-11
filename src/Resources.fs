module Resources

let unwrapOr (opt: 'b option) (def: 'b): 'b =
    match opt with
    | Some value -> value
    | None -> def

let english =
    Map.empty.
        Add("CLICK_TO_START", "Click me").
        Add("CLICK_TO_START", "Click me").
        Add("DELETE_CARD_FROM_SESSION", "Delete card from session").
        Add("NO_CARDS_LEFT", "No cards left").
        Add("CARDS_PLAYED", "Cards played").
        Add("NUMBER_CARDS_PLAYED", "Number of cards played so far").
        Add("ADD_PLAYER", "Add player").
        Add("PLAYER_SUSPEND_ON", "Suspend").
        Add("PLAYER_SUSPEND_OFF", "Unsuspend").
        Add("PLAYER_DELETE", "DELETE").
        Add("ACTIVE_CARD_DELETE", "Delete").
        Add("ACTIVE_CARD_USE", "Use").
        Add("RESET", "Reset").
        Add("CONTACT", "Contact").
        Add("SETTINGS", "Settings").
        Add("SETTINGS_MINIMUM_SIPS", "Minimum sips").
        Add("SETTINGS_MAXIMUM_SIPS", "Maximum sips").
        Add("SETTINGS_REMOTE", "Remote").
        Add("SETTINGS_AUDIO", "Audio").
        Add("SETTINGS_SAVE", "Save").
        Add("SETTINGS_LANGUAGE", "Language").
        Add("NO_ACTIVE_PLAYER", "No active player").
        Add("ROUND", "Round").
        Add("PERSONAL_CARD_INDICATOR", "personal").
        Add("ACTIVE_CARD_ADD_NOTE", "Add note").
        Add("ACTIVE_CARD_SAVE_NOTE", "Save").
        Add("GOOGLE_PLAY_IMG_ALT", "Get it on Google Play")


let german =
    Map.empty.
        Add("CLICK_TO_START", "Klick mich").
        Add("DELETE_CARD_FROM_SESSION", "Lösche Karte aus dem Spiel").
        Add("NO_CARDS_LEFT", "Keine Karten übrig").
        Add("CARDS_PLAYED", "Karten gespielt").
        Add("NUMBER_CARDS_PLAYED", "Anzahl bisher gespielter Karten").
        Add("ADD_PLAYER", "Spieler hinzufügen").
        Add("PLAYER_SUSPEND_ON", "Aussetzen").
        Add("PLAYER_SUSPEND_OFF", "Unsuspend").
        Add("PLAYER_DELETE", "Löschen").
        Add("ACTIVE_CARD_DELETE", "Löschen").
        Add("ACTIVE_CARD_USE", "Einsetzen").
        Add("RESET", "Zurücksetzen").
        Add("CONTACT", "Kontakt").
        Add("SETTINGS", "Einstellungen").
        Add("SETTINGS_MINIMUM_SIPS", "Mindestanzahl Schlücke").
        Add("SETTINGS_MAXIMUM_SIPS", "Maximalanzahl Schlücke").
        Add("SETTINGS_REMOTE", "Remote").
        Add("SETTINGS_AUDIO", "Audio").
        Add("SETTINGS_SAVE", "Speichern").
        Add("SETTINGS_LANGUAGE", "Sprache").
        Add("NO_ACTIVE_PLAYER", "Kein aktiver Spieler").
        Add("ROUND", "Runde").
        Add("PERSONAL_CARD_INDICATOR", "persönlich").
        Add("ACTIVE_CARD_ADD_NOTE", "Notiz hinzufügen").
        Add("ACTIVE_CARD_SAVE_NOTE", "Speichern").
        Add("GOOGLE_PLAY_IMG_ALT", "Jetzt bei Google Play")

let getKey language key =
    unwrapOr (match language with
                | "de" -> german.TryFind key
                | _ -> english.TryFind key) "UNDEFINED"
