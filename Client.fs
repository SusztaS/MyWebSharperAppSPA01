namespace MyWebSharperAppSPA01

open WebSharper
open WebSharper.JavaScript
open WebSharper.UI
open WebSharper.UI.Client
open WebSharper.UI.Templating

[<JavaScript>]
module Client =

    type ViewState = AdatView | ListaView | SorsolasView

    type IndexTemplate = Template<"wwwroot/index.html", ClientLoad.FromDocument, ServerLoad.WhenChanged>

    let People = ListModel.FromSeq [ "John"; "Paul" ]

    [<SPAEntryPoint>]
    let Main () =
        let currentView = Var.Create AdatView
        let newName = Var.Create ""

        IndexTemplate.Main()
            .ShowAdat(fun _ -> currentView.Value <- AdatView)
            .ShowLista(fun _ -> currentView.Value <- ListaView)
            .ShowSorsolas(fun _ -> currentView.Value <- SorsolasView)

            .MainContent(
                currentView.View.Map(fun view ->
                    match view with
                    | AdatView ->
                        IndexTemplate.AdatView()
                            .ListContainer(
                                People.View.DocSeqCached(fun (name: string) ->
                                    IndexTemplate.ListItem().Name(name).Doc()
                                )
                            )
                            .Name(newName)
                            .Add(fun _ ->
                                if newName.Value.Trim() <> "" then
                                    People.Add(newName.Value)
                                    newName.Value <- ""
                            )
                            .Doc()
                    
                    | ListaView ->
                        Doc.Element "div" [] [ 
                            Doc.Element "h1" [Attr.Class "h3"] [ Doc.TextNode "Versenyzők listázása" ]
                            Doc.TextNode "Itt jelenik majd meg a végleges lista táblázata." 
                        ]
                    
                    | SorsolasView ->
                        Doc.Element "div" [] [ 
                            Doc.Element "h1" [Attr.Class "h3"] [ Doc.TextNode "Sorsolás" ]
                            Doc.TextNode "A sorsolási funkció hamarosan elérhető lesz." 
                        ]
                ) |> Doc.EmbedView
            )
            .Doc()
        |> Doc.RunById "main"