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

    [<Inline "$el && $el.files && $el.files.length > 0 ? $el.files[0] : null">]
    let getFirstFile (el: obj) : obj = X<obj>

    [<Inline "$x == null">]
    let isNull (x: obj) : bool = X<bool>

    [<Inline "var r = new FileReader(); r.onload = function(e) { $cont(String(r.result)); }; r.readAsText($file);">]
    let readFileAsText (file: obj) (cont: string -> unit) : unit = X<unit>

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
        | "male" | "m" | "férfi" | "ferfi" -> Male
        | "female" | "f" | "nő" | "no" -> Female
        | _ -> Male

    let isHeaderRow (cells: string list) =
        match cells with
        | firstName :: familyName :: _ ->
            let a = firstName.Trim().ToLower()
            let b = familyName.Trim().ToLower()
            a = "firstname" || a = "keresztnev" || b = "familyname" || b = "vezeteknev"
        | _ -> false

    let tryParseCompetitor (line: string) =
        let cells =
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

    let renderCompetitorRow (c: Competitor) =
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
        let currentView = Var.Create AdatView
        let uploadStatus = Var.Create "Várt oszlopok: FirstName,FamilyName,ClubName,Race,DateOfBirth,Gender,DateOfMedicalExamination"
        let competitorList : Var<Competitor list> =
            Var.Create []
        let competitorRowsView : View<Doc> =
            competitorList.View.Map(fun items ->
                items
                |> Seq.map renderCompetitorRow
                |> Doc.Concat
            )

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
                                let input = getElementById "csvFileInput"
                                let file = getFirstFile input
                                if isNull file then
                                    uploadStatus.Value <- "Nincs kiválasztott file."
                                else
                                    readFileAsText file (fun content ->
                                        let parsed = parseCsv content
                                        competitorList.Value <- parsed
                                        uploadStatus.Value <- "Betöltött versenyzők száma: " + string parsed.Length
                                    )
                            )
                            .Doc()

                    | ListaView ->
                        Doc.Element "div" [] [
                            Doc.Element "h1" [Attr.Class "h3 mb-4"] [Doc.TextNode "Versenyzők listázása"] :> Doc
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