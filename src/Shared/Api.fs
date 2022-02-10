module Shared.Api

open System

open Shared.Models

module Route =
    let builder typeName methodName =
        sprintf "/api/%s/%s" typeName methodName

type RespondentId = RespondentId of Guid
type InterviewId = InterviewId of Guid
type LogId = LogId of Guid

type ILogboekApi =
    { grantToestemming: unit -> Async<Result<RespondentId, string>>
      revokeToestemming: (RespondentId * InterviewId option) -> Async<Result<unit, string>>
      submitDemografisch: RespondentId -> Demografisch.Result -> Async<Result<unit, string>>
      submitInterview: InterviewId option -> (Demografisch.Result * Interview.Result) -> Async<Result<InterviewId, string>>
      submitLog: RespondentId -> Logboek.Result -> Async<Result<LogId, string>>
      deleteLog: (RespondentId * LogId) -> Async<Result<unit, string>> }
