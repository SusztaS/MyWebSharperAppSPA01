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

    [<Inline "var r = new FileReader(); r.onload = function() { $cont(String(r.result)); }; r.readAsText($file, 'UTF-8');">]
    let readFileAsText (file: obj) (cont: string -> unit) : unit = X<unit>

    [<Inline "Math.random()">]
    let jsRandom () : float = X<float>

    let genderText g =
        match g with
        | Male -> "Male"
        | Female -> "Female"

    let formatDate (d: DateTime) =
        d.ToString("yyyy-MM-dd")

    let splitLine (line: string) =
        if line.Contains(";") then
            line.Split(';') |> Array.toList
        else
            line.Split(',') |> Array.toList

    let parseGender (value: string) =
        match value.Trim().ToLower() with
        | "male" | "m" -> Male
        | "female" | "f" -> Female
        | _ -> Male

    let isHeaderRow (cells: string list) =
        match cells with
        | firstName :: familyName :: _ ->
            let a = firstName.Trim().ToLower()
            let b = familyName.Trim().ToLower()
            a = "firstname" || a = "first name" ||
            b = "familyname" || b = "family name"
        | _ -> false

    let tryParseCompetitor (line: string) =
        let cells =
            splitLine line
            |> List.map (fun x -> x.Trim())

        match cells with
        | [ firstName; familyName; clubName; race; dateOfBirth; gender; medicalDate ] ->
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
        | _ -> None

    let parseCsv (content: string) =
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

    let shuffleList items =
        let arr = items |> List.toArray
        let mutable i = arr.Length - 1
        while i > 0 do
            let j = int (jsRandom() * float (i + 1))
            let tmp = arr.[i]
            arr.[i] <- arr.[j]
            arr.[j] <- tmp
            i <- i - 1
        arr |> Array.toList

    let shuffleWithinRaces items =
        items
        |> List.groupBy (fun c -> c.Race)
        |> List.collect (fun (_, x) -> shuffleList x)

    let renderCompetitorRow c =
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

    let renderDrawCell bg fg text =
        Doc.Element "td"
            [Attr.Create "style" ("background-color:" + bg + ";color:" + fg + ";font-weight:600")]
            [Doc.TextNode text] :> Doc

    let renderDrawMatchNumberCell bg fg matchNumber rowspan =
        Doc.Element "td"
            [
                Attr.Create "style" ("background-color:" + bg + ";color:" + fg + ";font-weight:700;vertical-align:middle;text-align:center;width:1%;white-space:nowrap;padding-left:8px;padding-right:8px")
                Attr.Create "rowspan" (string rowspan)
            ]
            [
                Doc.Element "div"
                    [
                        Attr.Create "style" "display:flex;align-items:center;justify-content:center;height:100%;min-height:80px;width:100%;text-align:center"
                    ]
                    [
                        Doc.TextNode (string matchNumber)
                    ] :> Doc
            ] :> Doc

    let renderDrawPair matchNumber pairIndex (pair: Competitor list) =
        let bg, fg =
            if pairIndex % 2 = 0 then
                "#e8f0ff", "#0a2a66"
            else
                "#f2f2f2", "#7a1f1f"

        match pair with
        | [a; b] ->
            Doc.Concat [
                Doc.Element "tr" [] [
                    renderDrawMatchNumberCell bg fg matchNumber 2
                    renderDrawCell bg fg (string a.Number)
                    renderDrawCell bg fg a.FirstName
                    renderDrawCell bg fg a.FamilyName
                    renderDrawCell bg fg a.ClubName
                ] :> Doc
                Doc.Element "tr" [] [
                    renderDrawCell bg fg (string b.Number)
                    renderDrawCell bg fg b.FirstName
                    renderDrawCell bg fg b.FamilyName
                    renderDrawCell bg fg b.ClubName
                ] :> Doc
            ]
        | [a] ->
            Doc.Element "tr" [] [
                renderDrawMatchNumberCell bg fg matchNumber 1
                renderDrawCell bg fg (string a.Number)
                renderDrawCell bg fg a.FirstName
                renderDrawCell bg fg a.FamilyName
                renderDrawCell bg fg a.ClubName
            ] :> Doc
        | _ ->
            Doc.Empty

    let renderRaceGroup (raceName, competitors) =
        let rows =
            competitors
            |> List.chunkBySize 2
            |> List.mapi (fun pairIndex pair ->
                renderDrawPair (pairIndex + 1) pairIndex pair)
            |> Doc.Concat

        Doc.Element "div" [Attr.Class "mb-5"] [
            Doc.Element "h2" [Attr.Class "h4 mb-3"] [Doc.TextNode raceName] :> Doc
            Doc.Element "table" [Attr.Class "table"; Attr.Create "style" "table-layout:fixed;width:100%"] [
                Doc.Element "colgroup" [] [
                    Doc.Element "col" [Attr.Create "style" "width:60px"] [] :> Doc
                    Doc.Element "col" [Attr.Create "style" "width:70px"] [] :> Doc
                    Doc.Element "col" [] [] :> Doc
                    Doc.Element "col" [] [] :> Doc
                    Doc.Element "col" [] [] :> Doc
                ] :> Doc
                Doc.Element "thead" [] [
                    Doc.Element "tr" [] [
                        Doc.Element "th" [Attr.Create "style" "width:60px;text-align:center;white-space:nowrap;padding-left:8px;padding-right:8px"] [Doc.TextNode "Match"] :> Doc
                        Doc.Element "th" [Attr.Create "style" "width:70px;white-space:nowrap"] [Doc.TextNode "#"] :> Doc
                        Doc.Element "th" [] [Doc.TextNode "First Name"] :> Doc
                        Doc.Element "th" [] [Doc.TextNode "Family Name"] :> Doc
                        Doc.Element "th" [] [Doc.TextNode "Club"] :> Doc
                    ] :> Doc
                ] :> Doc
                Doc.Element "tbody" [] [rows] :> Doc
            ] :> Doc
        ] :> Doc

    [<SPAEntryPoint>]
    let Main () =

        let currentView = Var.Create UploadView
        let drawDone = Var.Create false

        let uploadStatus =
            Var.Create "Expected CSV columns: FirstName,FamilyName,ClubName,Race,DateOfBirth,Gender,DateOfMedicalExamination"

        let selectedClub = Var.Create ""
        let selectedRace = Var.Create ""

        let competitorList : Var<Competitor list> = Var.Create []
        let drawList : Var<Competitor list> = Var.Create []

        let clubOptionsView =
            competitorList.View.Map(fun items ->
                items
                |> List.map (fun c -> c.ClubName)
                |> List.distinct
                |> List.sort
                |> List.map (fun club ->
                    Doc.Element "option" [Attr.Create "value" club] [Doc.TextNode club] :> Doc
                )
                |> Doc.Concat)

        let raceOptionsView =
            competitorList.View.Map(fun items ->
                items
                |> List.map (fun c -> c.Race)
                |> List.distinct
                |> List.sort
                |> List.map (fun race ->
                    Doc.Element "option" [Attr.Create "value" race] [Doc.TextNode race] :> Doc
                )
                |> Doc.Concat)

        let competitorRowsView =
            View.Map3 (fun items club race ->
                items
                |> List.filter (fun c ->
                    (club = "" || c.ClubName = club) &&
                    (race = "" || c.Race = race))
                |> Seq.map renderCompetitorRow
                |> Doc.Concat)
                competitorList.View
                selectedClub.View
                selectedRace.View

        let drawGroupsView =
            drawList.View.Map(fun items ->
                if List.isEmpty items then
                    Doc.Empty
                else
                    items
                    |> List.groupBy (fun c -> c.Race)
                    |> List.map renderRaceGroup
                    |> Doc.Concat)

        let drawButtonView =
            drawDone.View.Map(fun doneState ->
                if doneState then
                    Doc.Element "button" [
                        Attr.Class "btn btn-secondary"
                        Attr.Create "disabled" "disabled"
                    ] [
                        Doc.TextNode "Start draw"
                    ] :> Doc
                else
                    Doc.Element "button" [
                        Attr.Class "btn btn-primary"
                        on.click (fun _ _ ->
                            drawList.Value <- shuffleWithinRaces competitorList.Value
                            drawDone.Value <- true)
                    ] [
                        Doc.TextNode "Start draw"
                    ] :> Doc
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
                                    readFileAsText file (fun content ->
                                        let parsed = parseCsv content
                                        competitorList.Value <- parsed
                                        drawList.Value <- []
                                        drawDone.Value <- false
                                        selectedClub.Value <- ""
                                        selectedRace.Value <- ""
                                        uploadStatus.Value <- "Loaded competitors: " + string parsed.Length))
                            .Doc()

                    | ListView ->
                        Doc.Element "div" [] [

                            Doc.Element "h1" [Attr.Class "h3 mb-4"] [Doc.TextNode "Competitors list"] :> Doc

                            Doc.Element "div" [Attr.Class "row mb-3"] [

                                Doc.Element "div" [Attr.Class "col"] [
                                    Doc.Element "label" [Attr.Class "form-label"] [Doc.TextNode "Club"] :> Doc
                                    Doc.Element "select" [
                                        Attr.Class "form-select"
                                        Attr.Create "id" "clubFilter"
                                        on.change (fun _ _ -> selectedClub.Value <- getElementValue "clubFilter")
                                    ] [
                                        Doc.Element "option" [Attr.Create "value" ""] [Doc.TextNode "All"] :> Doc
                                        clubOptionsView |> Doc.EmbedView
                                    ] :> Doc
                                ] :> Doc

                                Doc.Element "div" [Attr.Class "col"] [
                                    Doc.Element "label" [Attr.Class "form-label"] [Doc.TextNode "Race"] :> Doc
                                    Doc.Element "select" [
                                        Attr.Class "form-select"
                                        Attr.Create "id" "raceFilter"
                                        on.change (fun _ _ -> selectedRace.Value <- getElementValue "raceFilter")
                                    ] [
                                        Doc.Element "option" [Attr.Create "value" ""] [Doc.TextNode "All"] :> Doc
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
                                        Doc.Element "th" [] [Doc.TextNode "Date Of Birth"] :> Doc
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

                                Doc.Element "h1" [Attr.Class "h3 mb-0"] [Doc.TextNode "Draw by race"] :> Doc

                                drawButtonView |> Doc.EmbedView

                            ] :> Doc

                            drawGroupsView |> Doc.EmbedView

                        ] :> Doc
                )
                |> Doc.EmbedView
            )
            .Doc()
        |> Doc.RunById "main"