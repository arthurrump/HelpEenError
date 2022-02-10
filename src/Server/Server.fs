module Server

open System

open System.Data
open Microsoft.Data.Sqlite
open Donald
open FsToolkit.ErrorHandling.ResultCE
open Fable.Remoting.Server
open Fable.Remoting.Giraffe
open Saturn

open Shared.Models
open Shared.Api

type Storage(conn: IDbConnection, ?tran: IDbTransaction) =

    let dbExec =
        match tran with
        | Some tran ->
            Db.setTransaction tran >> Db.exec
        | None ->
            Db.exec

    new (file, password) =
        let connStr = SqliteConnectionStringBuilder(
            DataSource = file,
            Mode = SqliteOpenMode.ReadWriteCreate,
            Cache = SqliteCacheMode.Shared,
            Password = password)
        let conn = new SqliteConnection(connStr.ToString())
        new Storage(conn)

    member __.Transaction (transact: Storage -> Result<'t, DbError>) =
        try
            use tran = conn.TryBeginTransaction()
            use storage = new Storage(conn, tran)
            let result = transact storage
            tran.TryCommit()
            result
        with
        | DbFailureException err -> Error err

    member __.Initialize () =
        let cmd = """
            CREATE TABLE IF NOT EXISTS respondenten (
                id TEXT PRIMARY KEY,
                schoolniveau TEXT,
                programmeervaardigheid INTEGER,
                programmeerervaring INTEGER
            );
            CREATE TABLE IF NOT EXISTS interviews (
                id TEXT PRIMARY KEY,
                schoolniveau TEXT NOT NULL,
                programmeervaardigheid INTEGER NOT NULL,
                programmeerervaring INTEGER NOT NULL,
                naam TEXT NOT NULL,
                klas TEXT NOT NULL,
                school TEXT NOT NULL,
                email TEXT NOT NULL,
                toestemming TEXT NOT NULL,
                emailouders TEXT,
                CONSTRAINT toestemming_emailouders CHECK (toestemming = 'alleen' OR emailouders NOT NULL)
            );
            CREATE TABLE IF NOT EXISTS logboek (
                id TEXT PRIMARY KEY,
                respondent_id TEXT NOT NULL REFERENCES respondenten(id) ON DELETE CASCADE,
                foutmelding TEXT,
                beschrijving TEXT,
                verwachting TEXT,
                vervolgactie TEXT NOT NULL,
                CONSTRAINT fout_beschrijving_and_verwachting CHECK (beschrijving NOT NULL = verwachting NOT NULL),
                CONSTRAINT fout_xor_foutmelding CHECK (foutmelding NOT NULL <> beschrijving NOT NULL)
            );
        """
        conn |> Db.newCommand cmd |> dbExec

    member __.AddRespondent (RespondentId id, demografisch: Demografisch.Result) =
        conn
        |> Db.newCommand """
            INSERT INTO respondenten (id, schoolniveau, programmeervaardigheid, programmeerervaring)
            VALUES (@id, @schoolniveau, @programmeervaardigheid, @programmeerervaring)"""
        |> Db.setParams
            [ "id", SqlType.String (id.ToString())
              "schoolniveau", SqlType.String demografisch.Schoolniveau
              "programmeervaardigheid", SqlType.Int demografisch.ProgrammeerVaardigheid
              "programmeerervaring", SqlType.Int demografisch.ProgrammeerErvaring ]
        |> dbExec

    member __.DeleteRespondent (RespondentId id) =
        conn
        |> Db.newCommand "DELETE FROM respondenten WHERE id = @id"
        |> Db.setParams [ "id", SqlType.String (id.ToString()) ]
        |> dbExec

    member __.AddOrReplaceInterview (InterviewId id, demografisch: Demografisch.Result, interview: Interview.Contact) =
        conn
        |> Db.newCommand """
            INSERT OR REPLACE INTO interviews (id, schoolniveau, programmeervaardigheid, programmeerervaring, naam, klas, school, email, toestemming, emailouders)
            VALUES (@id, @schoolniveau, @programmeervaardigheid, @programmeerervaring, @naam, @klas, @school, @email, @toestemming, @emailouders)"""
        |> Db.setParams
            [ "id", SqlType.String (id.ToString())
              "schoolniveau", SqlType.String demografisch.Schoolniveau
              "programmeervaardigheid", SqlType.Int demografisch.ProgrammeerVaardigheid
              "programmeerervaring", SqlType.Int demografisch.ProgrammeerErvaring
              "naam", SqlType.String interview.Naam
              "klas", SqlType.String interview.Klas
              "school", SqlType.String interview.School
              "email", SqlType.String interview.Email
              "toestemming", SqlType.String (
                match interview.Toestemming with
                | Interview.Toestemming.Alleen -> "alleen"
                | Interview.Toestemming.Ouders _ -> "ouders")
              "emailouders", (
                match interview.Toestemming with
                | Interview.Toestemming.Alleen -> SqlType.Null
                | Interview.Toestemming.Ouders emailouders -> SqlType.String emailouders) ]
        |> dbExec

    member __.DeleteInterview (InterviewId id) =
        conn
        |> Db.newCommand "DELETE FROM interviews WHERE id = @id"
        |> Db.setParams [ "id", SqlType.String (id.ToString()) ]
        |> dbExec

    member __.AddLog (LogId id, RespondentId respondentId, logboek: Logboek.Result) =
        conn
        |> Db.newCommand """
            INSERT INTO logboek (id, respondent_id, foutmelding, beschrijving, verwachting, vervolgactie)
            VALUES (@id, @respondent_id, @foutmelding, @beschrijving, @verwachting, @vervolgactie)"""
        |> Db.setParams
            [ "id", SqlType.String (id.ToString())
              "respondent_id", SqlType.String (respondentId.ToString())
              match logboek with
              | Logboek.Foutmelding (melding, vervolgactie) ->
                  "foutmelding", SqlType.String melding
                  "beschrijving", SqlType.Null
                  "verwachting", SqlType.Null
                  "vervolgactie", SqlType.String vervolgactie
              | Logboek.OnverwachtGedrag (beschrijving, verwachting, vervolgactie) ->
                  "foutmelding", SqlType.Null
                  "beschrijving", SqlType.String beschrijving
                  "verwachting", SqlType.String verwachting
                  "vervolgactie", SqlType.String vervolgactie ]
        |> dbExec

    member __.DeleteLog (LogId id, RespondentId respondentId) =
        conn
        |> Db.newCommand "DELETE FROM logboek WHERE id = @id AND respondent_id = @respondent_id"
        |> Db.setParams
            [ "id", SqlType.String (id.ToString())
              "respondent_id", SqlType.String (respondentId.ToString()) ]
        |> dbExec

    interface IDisposable with
        member __.Dispose() =
            conn.Close()
            tran |> Option.iter (fun tran -> tran.Dispose ())

let storage = new Storage(":memory:", "test")

storage.Initialize()
|> ignore

let dbErrorMessage = function
    | DbConnectionError _
    | DbTransactionError _ ->
        "Oeps, er is iets fout gegaan. Probeer het later nog eens."
    | DbExecutionError err ->
        $"Oeps, de database geeft een foutmelding: %s{err.Error.Message}"
    | DataReaderCastError _
    | DataReaderOutOfRangeError _ ->
        "Oeps, er is iets fout gegaan bij het lezen uit de database. Probeer het later nog eens."

let api =
    let grantToestemming () =
        async {
            return Ok (RespondentId (Guid()))
        }
    let revokeToestemming (respondentId, interviewIdOpt) =
        async {
            return storage.Transaction (fun storage ->
                result {
                    let! _ = storage.DeleteRespondent respondentId
                    let! _ =
                        match interviewIdOpt with
                        | Some interviewId -> storage.DeleteInterview interviewId
                        | None -> Ok ()
                    return ()
                })
            |> Result.mapError dbErrorMessage
        }
    let submitDemografisch respondentId demografisch =
        async {
            return storage.AddRespondent (respondentId, demografisch)
            |> Result.mapError dbErrorMessage
        }
    let submitInterview interviewIdOpt (demografisch, interview) =
        async {
            return result {
                match interviewIdOpt, interview with
                | Some interviewId, Interview.Result.Ja contact ->
                    do! storage.AddOrReplaceInterview (interviewId, demografisch, contact)
                    return interviewId
                | Some interviewId, Interview.Result.Nee ->
                    do! storage.DeleteInterview interviewId
                    return interviewId
                | None, Interview.Result.Ja contact ->
                    let interviewId = InterviewId (Guid())
                    do! storage.AddOrReplaceInterview (interviewId, demografisch, contact)
                    return interviewId
                | None, Interview.Result.Nee ->
                    return InterviewId (Guid())
            } |> Result.mapError dbErrorMessage
        }
    let submitLog respondentId logboek =
        async {
            return result {
                let logId = LogId (Guid())
                do! storage.AddLog (logId, respondentId, logboek)
                return logId
            } |> Result.mapError dbErrorMessage
        }
    let deleteLog (respondentId, logId) =
        async {
            return storage.DeleteLog (logId, respondentId)
            |> Result.mapError dbErrorMessage
        }
    { grantToestemming = grantToestemming
      revokeToestemming = revokeToestemming
      submitDemografisch = submitDemografisch
      submitInterview = submitInterview
      submitLog = submitLog
      deleteLog = deleteLog }

let webApp =
    Remoting.createApi ()
    |> Remoting.withRouteBuilder Route.builder
    |> Remoting.fromValue api
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
