namespace MyWebSharperAppSPA01

open WebSharper
open WebSharper.JavaScript
open WebSharper.UI
open WebSharper.UI.Client
open WebSharper.UI.Templating
open System

module Competitors =
    
    type Gender = 
        | Male
        | Female

    type Competitor =
        {
            Number: int
            FirstName: string
            FamilyName: string
            ClubName: string
            Race: string
            DateOfBirth: DateTime
            Gender: Gender
            DateOfMedicalExamination: DateTime
        }
        member this.DisplayName = this.FirstName + " " + this.FamilyName

[<JavaScript>]
module Client =

    open Competitors

    type ViewState =
        | UploadView
        | ListView
        | DrawView

    type IndexTemplate = Template<"wwwroot/index.html", ClientLoad.FromDocument, ServerLoad.WhenChanged>

    [<Inline "document.getElementById($id)">]
    let getElementById (id: string) : obj = X<obj>

    [<Inline "document.getElementById($id) ? document.getElementById($id).value : ''">]
    let getElementValue (id: string) : string = X<string>

    [<Inline "$el && $el.files && $el.files.length > 0 ? $el.files[0] : null">]
    let getFirstFile (el: obj) : obj = X<obj>

    [<Inline "$x == null">]
    let isNull (x: obj) : bool = X<bool>

    [<Inline "var r = new FileReader(); r.onload = function() { $cont(String(r.result)); }; r.onerror = function() { $onError(); }; r.readAsText($file);">]
    let readFileAsText (file: obj) (cont: string -> unit) (onError: unit -> unit) : unit = X<unit>

    [<Inline "Math.random()">]
    let jsRandom () : float = X<float>

    let genderText (g: Gender) : string =
        match g with
        | Male -> "Male"
        | Female -> "Female"

    let formatDate (d: DateTime) : string =
        d.ToString("yyyy-MM-dd")

    let splitLine (line: string) : string list =
        if line.Contains(";") then
            line.Split(';') |> Array.toList
        else
            line.Split(',') |> Array.toList

    let parseGender (value: string) : Gender =
        match value.Trim().ToLower() with
        | "male" | "m" | "férfi" | "ferfi" -> Male
        | "female" | "f" | "nő" | "no" -> Female
        | _ -> Male

    let isHeaderRow (cells: string list) : bool =
        match cells with
        | firstName :: familyName :: _ ->
            let a = firstName.Trim().ToLower()
            let b = familyName.Trim().ToLower()
            a = "firstname" || a = "first name" || a = "keresztnev" ||
            b = "familyname" || b = "family name" || b = "lastname" || b = "surname" || b = "vezeteknev"
        | _ -> false

    let tryParseCompetitor (line: string) : Competitor option =
        let cells =
            splitLine line
            |> List.map (fun x -> x.Trim())

        match cells with
        | [ firstName; familyName; clubName; race; dateOfBirth; gender; medicalDate ] ->
            try
                Some {
                    Number = 0
                    FirstName = firstName
                    FamilyName = familyName
                    ClubName = clubName
                    Race = race
                    DateOfBirth = DateTime.Parse(dateOfBirth)
                    Gender = parseGender gender
                    DateOfMedicalExamination = DateTime.Parse(medicalDate)
                }
            with
            | _ -> None
        | _ -> None

    let parseCsv (content: string) : Competitor list =
        content.Replace("\r", "").Split('\n')
        |> Array.toList
        |> List.map (fun x -> x.Trim())
        |> List.filter (fun x -> x <> "")
        |> fun lines ->
            match lines with
            | first :: rest when isHeaderRow (splitLine first) -> rest
            | _ -> lines
        |> List.choose tryParseCompetitor
        |> List.mapi (fun i c -> { c with Number = i + 1 })

    let shuffleList (items: 'T list) : 'T list =
        let arr = items |> List.toArray
        let mutable i = arr.Length - 1
        while i > 0 do
            let j = int (jsRandom() * float (i + 1))
            let temp = arr.[i]
            arr.[i] <- arr.[j]
            arr.[j] <- temp
            i <- i - 1
        arr |> Array.toList

    let shuffleWithinRaces (items: Competitor list) : Competitor list =
        items
        |> List.groupBy (fun c -> c.Race)
        |> List.sortBy fst
        |> List.collect (fun (_, groupItems) -> shuffleList groupItems)

    let renderCompetitorRow (c: Competitor) : Doc =
        Doc.Element "tr" [] [
            Doc.Element "td" [] [Doc.TextNode (string c.Number)] :> Doc
            Doc.Element "td" [] [Doc.TextNode c.FirstName] :> Doc
            Doc.Element "td" [] [Doc.TextNode c.FamilyName] :> Doc
            Doc.Element "td" [] [Doc.TextNode c.ClubName] :> Doc
            Doc.Element "td" [] [Doc.TextNode c.Race] :> Doc
            Doc.Element "td" [] [Doc.TextNode (formatDate c.DateOfBirth)] :> Doc
            Doc.Element "td" [] [Doc.TextNode (genderText c.Gender)] :> Doc
            Doc.Element "td" [] [Doc.TextNode (formatDate c.DateOfMedicalExamination)] :> Doc
        ] :> Doc

    let renderDrawRow (c: Competitor) : Doc =
        Doc.Element "tr" [] [
            Doc.Element "td" [] [Doc.TextNode (string c.Number)] :> Doc
            Doc.Element "td" [] [Doc.TextNode c.FirstName] :> Doc
            Doc.Element "td" [] [Doc.TextNode c.FamilyName] :> Doc
            Doc.Element "td" [] [Doc.TextNode c.ClubName] :> Doc
        ] :> Doc

    let renderRaceGroup (raceName: string, competitors: Competitor list) : Doc =
        let rows =
            competitors
            |> List.map renderDrawRow
            |> Doc.Concat

        Doc.Element "div" [Attr.Class "mb-5"] [
            Doc.Element "h2" [Attr.Class "h4 mb-3"] [Doc.TextNode raceName] :> Doc
            Doc.Element "table" [Attr.Class "table table-striped"] [
                Doc.Element "thead" [] [
                    Doc.Element "tr" [] [
                        Doc.Element "th" [] [Doc.TextNode "#"] :> Doc
                        Doc.Element "th" [] [Doc.TextNode "First Name"] :> Doc
                        Doc.Element "th" [] [Doc.TextNode "Family Name"] :> Doc
                        Doc.Element "th" [] [Doc.TextNode "Club"] :> Doc
                    ] :> Doc
                ] :> Doc
                Doc.Element "tbody" [] [
                    rows
                ] :> Doc
            ] :> Doc
        ] :> Doc

    [<SPAEntryPoint>]
    let Main () =

        let currentView : Var<ViewState> = Var.Create UploadView

        let uploadStatus : Var<string> =
            Var.Create "Expected CSV columns: FirstName,FamilyName,ClubName,Race,DateOfBirth,Gender,DateOfMedicalExamination"

        let selectedClub : Var<string> = Var.Create ""
        let selectedRace : Var<string> = Var.Create ""

        let competitorList : Var<Competitor list> = Var.Create []
        let drawList : Var<Competitor list> = Var.Create []

        let clubOptionsView : View<Doc> =
            competitorList.View.Map(fun items ->
                items
                |> List.map (fun c -> c.ClubName)
                |> List.distinct
                |> List.sort
                |> List.map (fun club ->
                    Doc.Element "option" [Attr.Create "value" club] [Doc.TextNode club] :> Doc
                )
                |> Doc.Concat
            )

        let raceOptionsView : View<Doc> =
            competitorList.View.Map(fun items ->
                items
                |> List.map (fun c -> c.Race)
                |> List.distinct
                |> List.sort
                |> List.map (fun race ->
                    Doc.Element "option" [Attr.Create "value" race] [Doc.TextNode race] :> Doc
                )
                |> Doc.Concat
            )

        let competitorRowsView : View<Doc> =
            View.Map3 (fun items club race ->
                items
                |> List.filter (fun c ->
                    (club = "" || c.ClubName = club) &&
                    (race = "" || c.Race = race)
                )
                |> Seq.map renderCompetitorRow
                |> Doc.Concat
            ) competitorList.View selectedClub.View selectedRace.View

        let drawGroupsView : View<Doc> =
            drawList.View.Map(fun items ->
                items
                |> List.groupBy (fun c -> c.Race)
                |> List.sortBy fst
                |> List.map renderRaceGroup
                |> Doc.Concat
            )

        IndexTemplate.Main()
            .ShowAdat(fun _ -> currentView.Value <- UploadView)
            .ShowLista(fun _ -> currentView.Value <- ListView)
            .ShowSorsolas(fun _ -> currentView.Value <- DrawView)
            .MainContent(
                currentView.View.Map(fun view ->
                    match view with

                    | UploadView ->
                        IndexTemplate.AdatView()
                            .UploadStatus(Doc.TextView uploadStatus.View)
                            .LoadCsv(fun _ ->
                                let input = getElementById "csvFileInput"
                                let file = getFirstFile input

                                if isNull file then
                                    uploadStatus.Value <- "No file selected."
                                else
                                    readFileAsText file
                                        (fun content ->
                                            let parsed = parseCsv content
                                            competitorList.Value <- parsed
                                            drawList.Value <- parsed
                                            selectedClub.Value <- ""
                                            selectedRace.Value <- ""

                                            if List.isEmpty parsed then
                                                uploadStatus.Value <- "No valid rows found in CSV."
                                            else
                                                uploadStatus.Value <- "Loaded competitors: " + string parsed.Length
                                        )
                                        (fun () ->
                                            uploadStatus.Value <- "File read error."
                                        )
                            )
                            .Doc()

                    | ListView ->
                        Doc.Element "div" [] [
                            Doc.Element "h1" [Attr.Class "h3 mb-4"] [Doc.TextNode "Competitor List"] :> Doc

                            Doc.Element "div" [Attr.Class "row mb-3"] [
                                Doc.Element "div" [Attr.Class "col"] [
                                    Doc.Element "label" [Attr.Class "form-label"] [Doc.TextNode "Filter by club"] :> Doc
                                    Doc.Element "select" [
                                        Attr.Class "form-select"
                                        Attr.Create "id" "clubFilter"
                                        on.change (fun _ _ ->
                                            selectedClub.Value <- getElementValue "clubFilter"
                                        )
                                    ] [
                                        Doc.Element "option" [Attr.Create "value" ""] [Doc.TextNode "All clubs"] :> Doc
                                        clubOptionsView |> Doc.EmbedView
                                    ] :> Doc
                                ] :> Doc

                                Doc.Element "div" [Attr.Class "col"] [
                                    Doc.Element "label" [Attr.Class "form-label"] [Doc.TextNode "Filter by race"] :> Doc
                                    Doc.Element "select" [
                                        Attr.Class "form-select"
                                        Attr.Create "id" "raceFilter"
                                        on.change (fun _ _ ->
                                            selectedRace.Value <- getElementValue "raceFilter"
                                        )
                                    ] [
                                        Doc.Element "option" [Attr.Create "value" ""] [Doc.TextNode "All races"] :> Doc
                                        raceOptionsView |> Doc.EmbedView
                                    ] :> Doc
                                ] :> Doc
                            ] :> Doc

                            Doc.Element "table" [Attr.Class "table table-striped"] [
                                Doc.Element "thead" [] [
                                    Doc.Element "tr" [] [
                                        Doc.Element "th" [] [Doc.TextNode "#"] :> Doc
                                        Doc.Element "th" [] [Doc.TextNode "First Name"] :> Doc
                                        Doc.Element "th" [] [Doc.TextNode "Family Name"] :> Doc
                                        Doc.Element "th" [] [Doc.TextNode "Club"] :> Doc
                                        Doc.Element "th" [] [Doc.TextNode "Race"] :> Doc
                                        Doc.Element "th" [] [Doc.TextNode "Date of Birth"] :> Doc
                                        Doc.Element "th" [] [Doc.TextNode "Gender"] :> Doc
                                        Doc.Element "th" [] [Doc.TextNode "Medical Date"] :> Doc
                                    ] :> Doc
                                ] :> Doc

                                Doc.Element "tbody" [] [
                                    competitorRowsView |> Doc.EmbedView
                                ] :> Doc
                            ] :> Doc
                        ] :> Doc

                    | DrawView ->
                        Doc.Element "div" [] [
                            Doc.Element "div" [Attr.Class "d-flex justify-content-between align-items-center mb-4"] [
                                Doc.Element "h1" [Attr.Class "h3 mb-0"] [Doc.TextNode "Draw by Race"] :> Doc
                                Doc.Element "button" [
                                    Attr.Class "btn btn-primary"
                                    on.click (fun _ _ ->
                                        drawList.Value <- shuffleWithinRaces drawList.Value
                                    )
                                ] [
                                    Doc.TextNode "Randomize within races"
                                ] :> Doc
                            ] :> Doc
                            drawGroupsView |> Doc.EmbedView
                        ] :> Doc
                ) |> Doc.EmbedView
            )
            .Doc()
        |> Doc.RunById "main"