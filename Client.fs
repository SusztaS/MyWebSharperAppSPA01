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
        | AdatView
        | ListaView
        | SorsolasView

    type IndexTemplate = Template<"wwwroot/index.html", ClientLoad.FromDocument, ServerLoad.WhenChanged>

    [<Inline "document.getElementById($id)">]
    let getElementById (id: string) : obj = X<obj>

    [<Inline "document.getElementById($id) ? document.getElementById($id).value : ''">]
    let getElementValue (id: string) : string = X<string>

    [<Inline "$el && $el.files && $el.files.length > 0 ? $el.files[0] : null">]
    let getFirstFile (el: obj) : obj = X<obj>

    [<Inline "$x == null">]
    let isNull (x: obj) : bool = X<bool>

    [<Inline "var r = new FileReader(); r.onload = function(e) { $cont(String(r.result)); }; r.readAsText($file);">]
    let readFileAsText (file: obj) (cont: string -> unit) : unit = X<unit>

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
            a = "firstname" || a = "keresztnev" || b = "familyname" || b = "vezeteknev"
        | _ -> false

    let tryParseCompetitor (line: string) : Competitor option =
        let cells : string list =
            splitLine line
            |> List.map (fun x -> x.Trim())

        match cells with
        | [ firstName; familyName; clubName; race; dateOfBirth; gender; dateOfMedicalExamination ] ->
            Some {
                FirstName = firstName
                FamilyName = familyName
                ClubName = clubName
                Race = race
                DateOfBirth = DateTime.Parse(dateOfBirth)
                Gender = parseGender gender
                DateOfMedicalExamination = DateTime.Parse(dateOfMedicalExamination)
            }
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

    let renderCompetitorRow (c: Competitor) : Doc =
        Doc.Element "tr" [] [
            Doc.Element "td" [] [Doc.TextNode c.FirstName] :> Doc
            Doc.Element "td" [] [Doc.TextNode c.FamilyName] :> Doc
            Doc.Element "td" [] [Doc.TextNode c.ClubName] :> Doc
            Doc.Element "td" [] [Doc.TextNode c.Race] :> Doc
            Doc.Element "td" [] [Doc.TextNode (formatDate c.DateOfBirth)] :> Doc
            Doc.Element "td" [] [Doc.TextNode (genderText c.Gender)] :> Doc
            Doc.Element "td" [] [Doc.TextNode (formatDate c.DateOfMedicalExamination)] :> Doc
        ] :> Doc

    [<SPAEntryPoint>]
    let Main () =
        let currentView : Var<ViewState> = Var.Create AdatView
        let uploadStatus : Var<string> = Var.Create "Várt oszlopok: FirstName,FamilyName,ClubName,Race,DateOfBirth,Gender,DateOfMedicalExamination"
        let selectedClub : Var<string> = Var.Create ""
        let competitorList : Var<Competitor list> = Var.Create []

        let clubOptionsView : View<Doc> =
            competitorList.View.Map(fun (items: Competitor list) ->
                items
                |> List.map (fun c -> c.ClubName)
                |> List.distinct
                |> List.sort
                |> List.map (fun (club: string) ->
                    Doc.Element "option" [Attr.Create "value" club] [Doc.TextNode club] :> Doc
                )
                |> Doc.Concat
            )

        let competitorRowsView : View<Doc> =
            View.Map2 (fun (items: Competitor list) (clubFilter: string) ->
                items
                |> List.filter (fun c -> clubFilter = "" || c.ClubName = clubFilter)
                |> Seq.map renderCompetitorRow
                |> Doc.Concat
            ) competitorList.View selectedClub.View

        IndexTemplate.Main()
            .ShowAdat(fun _ -> currentView.Value <- AdatView)
            .ShowLista(fun _ -> currentView.Value <- ListaView)
            .ShowSorsolas(fun _ -> currentView.Value <- SorsolasView)
            .MainContent(
                currentView.View.Map(fun view ->
                    match view with
                    | AdatView ->
                        IndexTemplate.AdatView()
                            .UploadStatus(Doc.TextView uploadStatus.View)
                            .LoadCsv(fun _ ->
                                let input : obj = getElementById "csvFileInput"
                                let file : obj = getFirstFile input
                                if isNull file then
                                    uploadStatus.Value <- "Nincs kiválasztott file."
                                else
                                    readFileAsText file (fun (content: string) ->
                                        let parsed : Competitor list = parseCsv content
                                        competitorList.Value <- parsed
                                        selectedClub.Value <- ""
                                        uploadStatus.Value <- "Betöltött versenyzők száma: " + string parsed.Length
                                    )
                            )
                            .Doc()

                    | ListaView ->
                        Doc.Element "div" [] [
                            Doc.Element "h1" [Attr.Class "h3 mb-4"] [Doc.TextNode "Versenyzők listázása"] :> Doc
                            Doc.Element "div" [Attr.Class "mb-3"] [
                                Doc.Element "label" [Attr.Class "form-label"] [Doc.TextNode "Szűrés club szerint"] :> Doc
                                Doc.Element "select" [
                                    Attr.Class "form-select"
                                    Attr.Create "id" "clubFilterSelect"
                                    on.change (fun _ _ ->
                                        let value : string = getElementValue "clubFilterSelect"
                                        selectedClub.Value <- value
                                    )
                                ] [
                                    Doc.Element "option" [Attr.Create "value" ""] [Doc.TextNode "Összes club"] :> Doc
                                    clubOptionsView |> Doc.EmbedView
                                ] :> Doc
                            ] :> Doc
                            Doc.Element "table" [Attr.Class "table table-striped"] [
                                Doc.Element "thead" [] [
                                    Doc.Element "tr" [] [
                                        Doc.Element "th" [] [Doc.TextNode "First Name"] :> Doc
                                        Doc.Element "th" [] [Doc.TextNode "Family Name"] :> Doc
                                        Doc.Element "th" [] [Doc.TextNode "Club Name"] :> Doc
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

                    | SorsolasView ->
                        Doc.Element "div" [] [
                            Doc.Element "h1" [Attr.Class "h3"] [Doc.TextNode "Sorsolás"] :> Doc
                            Doc.TextNode "A sorsolási funkció hamarosan elérhető lesz."
                        ] :> Doc
                ) |> Doc.EmbedView
            )
            .Doc()
        |> Doc.RunById "main"