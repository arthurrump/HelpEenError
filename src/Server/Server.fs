module Server

open System

open System.Data
open Microsoft.Data.Sqlite
open Donald

open Fable.Remoting.Server
open Fable.Remoting.Giraffe
open Saturn

open Shared

module Todo =
    let ofDataReader (rd: IDataReader) : Todo =
        { Id = rd.ReadGuid "id"
          Description = rd.ReadString "description" }

type Storage(file, password) =
    let connStr = SqliteConnectionStringBuilder(
        DataSource = file,
        Mode = SqliteOpenMode.ReadWriteCreate,
        Cache = SqliteCacheMode.Shared,
        Password = password)

    let conn = new SqliteConnection(connStr.ToString())

    member __.Initialize() =
        let cmd = "CREATE TABLE IF NOT EXISTS todos (id TEXT PRIMARY KEY, description TEXT)"
        conn |> Db.newCommand cmd |> Db.exec

    member __.GetTodos() =
        let sql = "SELECT id, description FROM todos"
        conn |> Db.newCommand sql |> Db.query Todo.ofDataReader |> Result.mapError string

    member __.AddTodo(todo: Todo) =
        if Todo.isValid todo.Description then
            let sql = "INSERT INTO todos (id, description) VALUES (@id, @description)"
            let param = [ "id", SqlType.Guid todo.Id; "description", SqlType.String todo.Description ]
            conn |> Db.newCommand sql |> Db.setParams param |> Db.exec |> Result.mapError string
        else
            Error "Invalid todo"

    interface IDisposable with
        member __.Dispose() =
            conn.Close()

let storage = new Storage(":memory:", "test")

storage.Initialize()
|> ignore

storage.AddTodo(Todo.create "Create new SAFE project")
|> ignore

storage.AddTodo(Todo.create "Write your app")
|> ignore

storage.AddTodo(Todo.create "Ship it !!!")
|> ignore

let todosApi =
    { getTodos = fun () ->
        async {
            match storage.GetTodos () with
            | Ok todos -> return todos
            | Error e -> printfn "%s" e; return failwith e
        }
      addTodo = fun todo ->
        async {
            match storage.AddTodo todo with
            | Ok () -> return todo
            | Error e -> return failwith e
        } }

let webApp =
    Remoting.createApi ()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.fromValue todosApi
    |> Remoting.buildHttpHandler

let app =
    application {
        url "http://0.0.0.0:8085"
        use_router webApp
        memory_cache
        use_static "public"
        use_gzip
    }

run app
