{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.SWF.Types.HistoryEvent
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SWF.Types.HistoryEvent where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SWF.Types.ActivityTaskCancelRequestedEventAttributes
import Amazonka.SWF.Types.ActivityTaskCanceledEventAttributes
import Amazonka.SWF.Types.ActivityTaskCompletedEventAttributes
import Amazonka.SWF.Types.ActivityTaskFailedEventAttributes
import Amazonka.SWF.Types.ActivityTaskScheduledEventAttributes
import Amazonka.SWF.Types.ActivityTaskStartedEventAttributes
import Amazonka.SWF.Types.ActivityTaskTimedOutEventAttributes
import Amazonka.SWF.Types.CancelTimerFailedEventAttributes
import Amazonka.SWF.Types.CancelWorkflowExecutionFailedEventAttributes
import Amazonka.SWF.Types.ChildWorkflowExecutionCanceledEventAttributes
import Amazonka.SWF.Types.ChildWorkflowExecutionCompletedEventAttributes
import Amazonka.SWF.Types.ChildWorkflowExecutionFailedEventAttributes
import Amazonka.SWF.Types.ChildWorkflowExecutionStartedEventAttributes
import Amazonka.SWF.Types.ChildWorkflowExecutionTerminatedEventAttributes
import Amazonka.SWF.Types.ChildWorkflowExecutionTimedOutEventAttributes
import Amazonka.SWF.Types.CompleteWorkflowExecutionFailedEventAttributes
import Amazonka.SWF.Types.ContinueAsNewWorkflowExecutionFailedEventAttributes
import Amazonka.SWF.Types.DecisionTaskCompletedEventAttributes
import Amazonka.SWF.Types.DecisionTaskScheduledEventAttributes
import Amazonka.SWF.Types.DecisionTaskStartedEventAttributes
import Amazonka.SWF.Types.DecisionTaskTimedOutEventAttributes
import Amazonka.SWF.Types.EventType
import Amazonka.SWF.Types.ExternalWorkflowExecutionCancelRequestedEventAttributes
import Amazonka.SWF.Types.ExternalWorkflowExecutionSignaledEventAttributes
import Amazonka.SWF.Types.FailWorkflowExecutionFailedEventAttributes
import Amazonka.SWF.Types.LambdaFunctionCompletedEventAttributes
import Amazonka.SWF.Types.LambdaFunctionFailedEventAttributes
import Amazonka.SWF.Types.LambdaFunctionScheduledEventAttributes
import Amazonka.SWF.Types.LambdaFunctionStartedEventAttributes
import Amazonka.SWF.Types.LambdaFunctionTimedOutEventAttributes
import Amazonka.SWF.Types.MarkerRecordedEventAttributes
import Amazonka.SWF.Types.RecordMarkerFailedEventAttributes
import Amazonka.SWF.Types.RequestCancelActivityTaskFailedEventAttributes
import Amazonka.SWF.Types.RequestCancelExternalWorkflowExecutionFailedEventAttributes
import Amazonka.SWF.Types.RequestCancelExternalWorkflowExecutionInitiatedEventAttributes
import Amazonka.SWF.Types.ScheduleActivityTaskFailedEventAttributes
import Amazonka.SWF.Types.ScheduleLambdaFunctionFailedEventAttributes
import Amazonka.SWF.Types.SignalExternalWorkflowExecutionFailedEventAttributes
import Amazonka.SWF.Types.SignalExternalWorkflowExecutionInitiatedEventAttributes
import Amazonka.SWF.Types.StartChildWorkflowExecutionFailedEventAttributes
import Amazonka.SWF.Types.StartChildWorkflowExecutionInitiatedEventAttributes
import Amazonka.SWF.Types.StartLambdaFunctionFailedEventAttributes
import Amazonka.SWF.Types.StartTimerFailedEventAttributes
import Amazonka.SWF.Types.TimerCanceledEventAttributes
import Amazonka.SWF.Types.TimerFiredEventAttributes
import Amazonka.SWF.Types.TimerStartedEventAttributes
import Amazonka.SWF.Types.WorkflowExecutionCancelRequestedEventAttributes
import Amazonka.SWF.Types.WorkflowExecutionCanceledEventAttributes
import Amazonka.SWF.Types.WorkflowExecutionCompletedEventAttributes
import Amazonka.SWF.Types.WorkflowExecutionContinuedAsNewEventAttributes
import Amazonka.SWF.Types.WorkflowExecutionFailedEventAttributes
import Amazonka.SWF.Types.WorkflowExecutionSignaledEventAttributes
import Amazonka.SWF.Types.WorkflowExecutionStartedEventAttributes
import Amazonka.SWF.Types.WorkflowExecutionTerminatedEventAttributes
import Amazonka.SWF.Types.WorkflowExecutionTimedOutEventAttributes

-- | Event within a workflow execution. A history event can be one of these
-- types:
--
-- -   @ActivityTaskCancelRequested@ – A @RequestCancelActivityTask@
--     decision was received by the system.
--
-- -   @ActivityTaskCanceled@ – The activity task was successfully
--     canceled.
--
-- -   @ActivityTaskCompleted@ – An activity worker successfully completed
--     an activity task by calling RespondActivityTaskCompleted.
--
-- -   @ActivityTaskFailed@ – An activity worker failed an activity task by
--     calling RespondActivityTaskFailed.
--
-- -   @ActivityTaskScheduled@ – An activity task was scheduled for
--     execution.
--
-- -   @ActivityTaskStarted@ – The scheduled activity task was dispatched
--     to a worker.
--
-- -   @ActivityTaskTimedOut@ – The activity task timed out.
--
-- -   @CancelTimerFailed@ – Failed to process CancelTimer decision. This
--     happens when the decision isn\'t configured properly, for example no
--     timer exists with the specified timer Id.
--
-- -   @CancelWorkflowExecutionFailed@ – A request to cancel a workflow
--     execution failed.
--
-- -   @ChildWorkflowExecutionCanceled@ – A child workflow execution,
--     started by this workflow execution, was canceled and closed.
--
-- -   @ChildWorkflowExecutionCompleted@ – A child workflow execution,
--     started by this workflow execution, completed successfully and was
--     closed.
--
-- -   @ChildWorkflowExecutionFailed@ – A child workflow execution, started
--     by this workflow execution, failed to complete successfully and was
--     closed.
--
-- -   @ChildWorkflowExecutionStarted@ – A child workflow execution was
--     successfully started.
--
-- -   @ChildWorkflowExecutionTerminated@ – A child workflow execution,
--     started by this workflow execution, was terminated.
--
-- -   @ChildWorkflowExecutionTimedOut@ – A child workflow execution,
--     started by this workflow execution, timed out and was closed.
--
-- -   @CompleteWorkflowExecutionFailed@ – The workflow execution failed to
--     complete.
--
-- -   @ContinueAsNewWorkflowExecutionFailed@ – The workflow execution
--     failed to complete after being continued as a new workflow
--     execution.
--
-- -   @DecisionTaskCompleted@ – The decider successfully completed a
--     decision task by calling RespondDecisionTaskCompleted.
--
-- -   @DecisionTaskScheduled@ – A decision task was scheduled for the
--     workflow execution.
--
-- -   @DecisionTaskStarted@ – The decision task was dispatched to a
--     decider.
--
-- -   @DecisionTaskTimedOut@ – The decision task timed out.
--
-- -   @ExternalWorkflowExecutionCancelRequested@ – Request to cancel an
--     external workflow execution was successfully delivered to the target
--     execution.
--
-- -   @ExternalWorkflowExecutionSignaled@ – A signal, requested by this
--     workflow execution, was successfully delivered to the target
--     external workflow execution.
--
-- -   @FailWorkflowExecutionFailed@ – A request to mark a workflow
--     execution as failed, itself failed.
--
-- -   @MarkerRecorded@ – A marker was recorded in the workflow history as
--     the result of a @RecordMarker@ decision.
--
-- -   @RecordMarkerFailed@ – A @RecordMarker@ decision was returned as
--     failed.
--
-- -   @RequestCancelActivityTaskFailed@ – Failed to process
--     RequestCancelActivityTask decision. This happens when the decision
--     isn\'t configured properly.
--
-- -   @RequestCancelExternalWorkflowExecutionFailed@ – Request to cancel
--     an external workflow execution failed.
--
-- -   @RequestCancelExternalWorkflowExecutionInitiated@ – A request was
--     made to request the cancellation of an external workflow execution.
--
-- -   @ScheduleActivityTaskFailed@ – Failed to process
--     ScheduleActivityTask decision. This happens when the decision isn\'t
--     configured properly, for example the activity type specified isn\'t
--     registered.
--
-- -   @SignalExternalWorkflowExecutionFailed@ – The request to signal an
--     external workflow execution failed.
--
-- -   @SignalExternalWorkflowExecutionInitiated@ – A request to signal an
--     external workflow was made.
--
-- -   @StartActivityTaskFailed@ – A scheduled activity task failed to
--     start.
--
-- -   @StartChildWorkflowExecutionFailed@ – Failed to process
--     StartChildWorkflowExecution decision. This happens when the decision
--     isn\'t configured properly, for example the workflow type specified
--     isn\'t registered.
--
-- -   @StartChildWorkflowExecutionInitiated@ – A request was made to start
--     a child workflow execution.
--
-- -   @StartTimerFailed@ – Failed to process StartTimer decision. This
--     happens when the decision isn\'t configured properly, for example a
--     timer already exists with the specified timer Id.
--
-- -   @TimerCanceled@ – A timer, previously started for this workflow
--     execution, was successfully canceled.
--
-- -   @TimerFired@ – A timer, previously started for this workflow
--     execution, fired.
--
-- -   @TimerStarted@ – A timer was started for the workflow execution due
--     to a @StartTimer@ decision.
--
-- -   @WorkflowExecutionCancelRequested@ – A request to cancel this
--     workflow execution was made.
--
-- -   @WorkflowExecutionCanceled@ – The workflow execution was
--     successfully canceled and closed.
--
-- -   @WorkflowExecutionCompleted@ – The workflow execution was closed due
--     to successful completion.
--
-- -   @WorkflowExecutionContinuedAsNew@ – The workflow execution was
--     closed and a new execution of the same type was created with the
--     same workflowId.
--
-- -   @WorkflowExecutionFailed@ – The workflow execution closed due to a
--     failure.
--
-- -   @WorkflowExecutionSignaled@ – An external signal was received for
--     the workflow execution.
--
-- -   @WorkflowExecutionStarted@ – The workflow execution was started.
--
-- -   @WorkflowExecutionTerminated@ – The workflow execution was
--     terminated.
--
-- -   @WorkflowExecutionTimedOut@ – The workflow execution was closed
--     because a time out was exceeded.
--
-- /See:/ 'newHistoryEvent' smart constructor.
data HistoryEvent = HistoryEvent'
  { -- | If the event is of type @WorkflowExecutionCancelRequested@ then this
    -- member is set and provides detailed information about the event. It
    -- isn\'t set for other event types.
    workflowExecutionCancelRequestedEventAttributes :: Prelude.Maybe WorkflowExecutionCancelRequestedEventAttributes,
    -- | If the event is of type @DecisionTaskFailed@ then this member is set and
    -- provides detailed information about the event. It isn\'t set for other
    -- event types.
    recordMarkerFailedEventAttributes :: Prelude.Maybe RecordMarkerFailedEventAttributes,
    -- | If the event is of type
    -- @RequestCancelExternalWorkflowExecutionInitiated@ then this member is
    -- set and provides detailed information about the event. It isn\'t set for
    -- other event types.
    requestCancelExternalWorkflowExecutionInitiatedEventAttributes :: Prelude.Maybe RequestCancelExternalWorkflowExecutionInitiatedEventAttributes,
    -- | Provides the details of the @LambdaFunctionStarted@ event. It isn\'t set
    -- for other event types.
    lambdaFunctionStartedEventAttributes :: Prelude.Maybe LambdaFunctionStartedEventAttributes,
    -- | If the event is of type @DecisionTaskScheduled@ then this member is set
    -- and provides detailed information about the event. It isn\'t set for
    -- other event types.
    decisionTaskScheduledEventAttributes :: Prelude.Maybe DecisionTaskScheduledEventAttributes,
    -- | If the event is of type @WorkflowExecutionCompleted@ then this member is
    -- set and provides detailed information about the event. It isn\'t set for
    -- other event types.
    workflowExecutionCompletedEventAttributes :: Prelude.Maybe WorkflowExecutionCompletedEventAttributes,
    -- | If the event is of type @StartTimerFailed@ then this member is set and
    -- provides detailed information about the event. It isn\'t set for other
    -- event types.
    startTimerFailedEventAttributes :: Prelude.Maybe StartTimerFailedEventAttributes,
    -- | If the event is of type @ActivityTaskScheduled@ then this member is set
    -- and provides detailed information about the event. It isn\'t set for
    -- other event types.
    activityTaskScheduledEventAttributes :: Prelude.Maybe ActivityTaskScheduledEventAttributes,
    -- | If the event is of type @ScheduleActivityTaskFailed@ then this member is
    -- set and provides detailed information about the event. It isn\'t set for
    -- other event types.
    scheduleActivityTaskFailedEventAttributes :: Prelude.Maybe ScheduleActivityTaskFailedEventAttributes,
    -- | If the event is of type @ChildWorkflowExecutionCompleted@ then this
    -- member is set and provides detailed information about the event. It
    -- isn\'t set for other event types.
    childWorkflowExecutionCompletedEventAttributes :: Prelude.Maybe ChildWorkflowExecutionCompletedEventAttributes,
    -- | If the event is of type @MarkerRecorded@ then this member is set and
    -- provides detailed information about the event. It isn\'t set for other
    -- event types.
    markerRecordedEventAttributes :: Prelude.Maybe MarkerRecordedEventAttributes,
    -- | Provides the details of the @ScheduleLambdaFunctionFailed@ event. It
    -- isn\'t set for other event types.
    scheduleLambdaFunctionFailedEventAttributes :: Prelude.Maybe ScheduleLambdaFunctionFailedEventAttributes,
    -- | If the event is of type @CompleteWorkflowExecutionFailed@ then this
    -- member is set and provides detailed information about the event. It
    -- isn\'t set for other event types.
    completeWorkflowExecutionFailedEventAttributes :: Prelude.Maybe CompleteWorkflowExecutionFailedEventAttributes,
    -- | Provides the details of the @LambdaFunctionCompleted@ event. It isn\'t
    -- set for other event types.
    lambdaFunctionCompletedEventAttributes :: Prelude.Maybe LambdaFunctionCompletedEventAttributes,
    -- | If the event is of type @RequestCancelExternalWorkflowExecutionFailed@
    -- then this member is set and provides detailed information about the
    -- event. It isn\'t set for other event types.
    requestCancelExternalWorkflowExecutionFailedEventAttributes :: Prelude.Maybe RequestCancelExternalWorkflowExecutionFailedEventAttributes,
    -- | If the event is of type @TimerCanceled@ then this member is set and
    -- provides detailed information about the event. It isn\'t set for other
    -- event types.
    timerCanceledEventAttributes :: Prelude.Maybe TimerCanceledEventAttributes,
    -- | If the event is of type @WorkflowExecutionStarted@ then this member is
    -- set and provides detailed information about the event. It isn\'t set for
    -- other event types.
    workflowExecutionStartedEventAttributes :: Prelude.Maybe WorkflowExecutionStartedEventAttributes,
    -- | If the event is of type @ActivityTaskCompleted@ then this member is set
    -- and provides detailed information about the event. It isn\'t set for
    -- other event types.
    activityTaskCompletedEventAttributes :: Prelude.Maybe ActivityTaskCompletedEventAttributes,
    -- | If the event is of type @DecisionTaskTimedOut@ then this member is set
    -- and provides detailed information about the event. It isn\'t set for
    -- other event types.
    decisionTaskTimedOutEventAttributes :: Prelude.Maybe DecisionTaskTimedOutEventAttributes,
    -- | If the event is of type @CancelTimerFailed@ then this member is set and
    -- provides detailed information about the event. It isn\'t set for other
    -- event types.
    cancelTimerFailedEventAttributes :: Prelude.Maybe CancelTimerFailedEventAttributes,
    -- | If the event is of type @ChildWorkflowExecutionStarted@ then this member
    -- is set and provides detailed information about the event. It isn\'t set
    -- for other event types.
    childWorkflowExecutionStartedEventAttributes :: Prelude.Maybe ChildWorkflowExecutionStartedEventAttributes,
    -- | If the event is of type @ActivityTaskCanceled@ then this member is set
    -- and provides detailed information about the event. It isn\'t set for
    -- other event types.
    activityTaskCanceledEventAttributes :: Prelude.Maybe ActivityTaskCanceledEventAttributes,
    -- | If the event is of type @ActivityTaskTimedOut@ then this member is set
    -- and provides detailed information about the event. It isn\'t set for
    -- other event types.
    activityTaskTimedOutEventAttributes :: Prelude.Maybe ActivityTaskTimedOutEventAttributes,
    -- | If the event is of type @DecisionTaskStarted@ then this member is set
    -- and provides detailed information about the event. It isn\'t set for
    -- other event types.
    decisionTaskStartedEventAttributes :: Prelude.Maybe DecisionTaskStartedEventAttributes,
    -- | If the event is of type @WorkflowExecutionTerminated@ then this member
    -- is set and provides detailed information about the event. It isn\'t set
    -- for other event types.
    workflowExecutionTerminatedEventAttributes :: Prelude.Maybe WorkflowExecutionTerminatedEventAttributes,
    -- | If the event is of type @ChildWorkflowExecutionCanceled@ then this
    -- member is set and provides detailed information about the event. It
    -- isn\'t set for other event types.
    childWorkflowExecutionCanceledEventAttributes :: Prelude.Maybe ChildWorkflowExecutionCanceledEventAttributes,
    -- | If the event is of type @RequestCancelActivityTaskFailed@ then this
    -- member is set and provides detailed information about the event. It
    -- isn\'t set for other event types.
    requestCancelActivityTaskFailedEventAttributes :: Prelude.Maybe RequestCancelActivityTaskFailedEventAttributes,
    -- | Provides the details of the @LambdaFunctionScheduled@ event. It isn\'t
    -- set for other event types.
    lambdaFunctionScheduledEventAttributes :: Prelude.Maybe LambdaFunctionScheduledEventAttributes,
    -- | If the event is of type @ChildWorkflowExecutionTimedOut@ then this
    -- member is set and provides detailed information about the event. It
    -- isn\'t set for other event types.
    childWorkflowExecutionTimedOutEventAttributes :: Prelude.Maybe ChildWorkflowExecutionTimedOutEventAttributes,
    -- | If the event is of type @CancelWorkflowExecutionFailed@ then this member
    -- is set and provides detailed information about the event. It isn\'t set
    -- for other event types.
    cancelWorkflowExecutionFailedEventAttributes :: Prelude.Maybe CancelWorkflowExecutionFailedEventAttributes,
    -- | If the event is of type @StartChildWorkflowExecutionInitiated@ then this
    -- member is set and provides detailed information about the event. It
    -- isn\'t set for other event types.
    startChildWorkflowExecutionInitiatedEventAttributes :: Prelude.Maybe StartChildWorkflowExecutionInitiatedEventAttributes,
    -- | If the event is of type @SignalExternalWorkflowExecutionFailed@ then
    -- this member is set and provides detailed information about the event. It
    -- isn\'t set for other event types.
    signalExternalWorkflowExecutionFailedEventAttributes :: Prelude.Maybe SignalExternalWorkflowExecutionFailedEventAttributes,
    -- | If the event is of type @ActivityTaskStarted@ then this member is set
    -- and provides detailed information about the event. It isn\'t set for
    -- other event types.
    activityTaskStartedEventAttributes :: Prelude.Maybe ActivityTaskStartedEventAttributes,
    -- | Provides the details of the @StartLambdaFunctionFailed@ event. It isn\'t
    -- set for other event types.
    startLambdaFunctionFailedEventAttributes :: Prelude.Maybe StartLambdaFunctionFailedEventAttributes,
    -- | If the event is of type @ChildWorkflowExecutionTerminated@ then this
    -- member is set and provides detailed information about the event. It
    -- isn\'t set for other event types.
    childWorkflowExecutionTerminatedEventAttributes :: Prelude.Maybe ChildWorkflowExecutionTerminatedEventAttributes,
    -- | Provides the details of the @LambdaFunctionFailed@ event. It isn\'t set
    -- for other event types.
    lambdaFunctionFailedEventAttributes :: Prelude.Maybe LambdaFunctionFailedEventAttributes,
    -- | If the event is of type @WorkflowExecutionCanceled@ then this member is
    -- set and provides detailed information about the event. It isn\'t set for
    -- other event types.
    workflowExecutionCanceledEventAttributes :: Prelude.Maybe WorkflowExecutionCanceledEventAttributes,
    -- | If the event is of type @TimerStarted@ then this member is set and
    -- provides detailed information about the event. It isn\'t set for other
    -- event types.
    timerStartedEventAttributes :: Prelude.Maybe TimerStartedEventAttributes,
    -- | If the event is of type @ActivityTaskcancelRequested@ then this member
    -- is set and provides detailed information about the event. It isn\'t set
    -- for other event types.
    activityTaskCancelRequestedEventAttributes :: Prelude.Maybe ActivityTaskCancelRequestedEventAttributes,
    -- | If the event is of type @WorkflowExecutionTimedOut@ then this member is
    -- set and provides detailed information about the event. It isn\'t set for
    -- other event types.
    workflowExecutionTimedOutEventAttributes :: Prelude.Maybe WorkflowExecutionTimedOutEventAttributes,
    -- | If the event is of type @WorkflowExecutionSignaled@ then this member is
    -- set and provides detailed information about the event. It isn\'t set for
    -- other event types.
    workflowExecutionSignaledEventAttributes :: Prelude.Maybe WorkflowExecutionSignaledEventAttributes,
    -- | If the event is of type @TimerFired@ then this member is set and
    -- provides detailed information about the event. It isn\'t set for other
    -- event types.
    timerFiredEventAttributes :: Prelude.Maybe TimerFiredEventAttributes,
    -- | If the event is of type @ActivityTaskFailed@ then this member is set and
    -- provides detailed information about the event. It isn\'t set for other
    -- event types.
    activityTaskFailedEventAttributes :: Prelude.Maybe ActivityTaskFailedEventAttributes,
    -- | If the event is of type @ExternalWorkflowExecutionSignaled@ then this
    -- member is set and provides detailed information about the event. It
    -- isn\'t set for other event types.
    externalWorkflowExecutionSignaledEventAttributes :: Prelude.Maybe ExternalWorkflowExecutionSignaledEventAttributes,
    -- | If the event is of type @DecisionTaskCompleted@ then this member is set
    -- and provides detailed information about the event. It isn\'t set for
    -- other event types.
    decisionTaskCompletedEventAttributes :: Prelude.Maybe DecisionTaskCompletedEventAttributes,
    -- | If the event is of type @StartChildWorkflowExecutionFailed@ then this
    -- member is set and provides detailed information about the event. It
    -- isn\'t set for other event types.
    startChildWorkflowExecutionFailedEventAttributes :: Prelude.Maybe StartChildWorkflowExecutionFailedEventAttributes,
    -- | If the event is of type @ChildWorkflowExecutionFailed@ then this member
    -- is set and provides detailed information about the event. It isn\'t set
    -- for other event types.
    childWorkflowExecutionFailedEventAttributes :: Prelude.Maybe ChildWorkflowExecutionFailedEventAttributes,
    -- | If the event is of type @FailWorkflowExecutionFailed@ then this member
    -- is set and provides detailed information about the event. It isn\'t set
    -- for other event types.
    failWorkflowExecutionFailedEventAttributes :: Prelude.Maybe FailWorkflowExecutionFailedEventAttributes,
    -- | If the event is of type @ContinueAsNewWorkflowExecutionFailed@ then this
    -- member is set and provides detailed information about the event. It
    -- isn\'t set for other event types.
    continueAsNewWorkflowExecutionFailedEventAttributes :: Prelude.Maybe ContinueAsNewWorkflowExecutionFailedEventAttributes,
    -- | If the event is of type @SignalExternalWorkflowExecutionInitiated@ then
    -- this member is set and provides detailed information about the event. It
    -- isn\'t set for other event types.
    signalExternalWorkflowExecutionInitiatedEventAttributes :: Prelude.Maybe SignalExternalWorkflowExecutionInitiatedEventAttributes,
    -- | Provides the details of the @LambdaFunctionTimedOut@ event. It isn\'t
    -- set for other event types.
    lambdaFunctionTimedOutEventAttributes :: Prelude.Maybe LambdaFunctionTimedOutEventAttributes,
    -- | If the event is of type @WorkflowExecutionFailed@ then this member is
    -- set and provides detailed information about the event. It isn\'t set for
    -- other event types.
    workflowExecutionFailedEventAttributes :: Prelude.Maybe WorkflowExecutionFailedEventAttributes,
    -- | If the event is of type @WorkflowExecutionContinuedAsNew@ then this
    -- member is set and provides detailed information about the event. It
    -- isn\'t set for other event types.
    workflowExecutionContinuedAsNewEventAttributes :: Prelude.Maybe WorkflowExecutionContinuedAsNewEventAttributes,
    -- | If the event is of type @ExternalWorkflowExecutionCancelRequested@ then
    -- this member is set and provides detailed information about the event. It
    -- isn\'t set for other event types.
    externalWorkflowExecutionCancelRequestedEventAttributes :: Prelude.Maybe ExternalWorkflowExecutionCancelRequestedEventAttributes,
    -- | The date and time when the event occurred.
    eventTimestamp :: Core.POSIX,
    -- | The type of the history event.
    eventType :: EventType,
    -- | The system generated ID of the event. This ID uniquely identifies the
    -- event with in the workflow execution history.
    eventId :: Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'HistoryEvent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'workflowExecutionCancelRequestedEventAttributes', 'historyEvent_workflowExecutionCancelRequestedEventAttributes' - If the event is of type @WorkflowExecutionCancelRequested@ then this
-- member is set and provides detailed information about the event. It
-- isn\'t set for other event types.
--
-- 'recordMarkerFailedEventAttributes', 'historyEvent_recordMarkerFailedEventAttributes' - If the event is of type @DecisionTaskFailed@ then this member is set and
-- provides detailed information about the event. It isn\'t set for other
-- event types.
--
-- 'requestCancelExternalWorkflowExecutionInitiatedEventAttributes', 'historyEvent_requestCancelExternalWorkflowExecutionInitiatedEventAttributes' - If the event is of type
-- @RequestCancelExternalWorkflowExecutionInitiated@ then this member is
-- set and provides detailed information about the event. It isn\'t set for
-- other event types.
--
-- 'lambdaFunctionStartedEventAttributes', 'historyEvent_lambdaFunctionStartedEventAttributes' - Provides the details of the @LambdaFunctionStarted@ event. It isn\'t set
-- for other event types.
--
-- 'decisionTaskScheduledEventAttributes', 'historyEvent_decisionTaskScheduledEventAttributes' - If the event is of type @DecisionTaskScheduled@ then this member is set
-- and provides detailed information about the event. It isn\'t set for
-- other event types.
--
-- 'workflowExecutionCompletedEventAttributes', 'historyEvent_workflowExecutionCompletedEventAttributes' - If the event is of type @WorkflowExecutionCompleted@ then this member is
-- set and provides detailed information about the event. It isn\'t set for
-- other event types.
--
-- 'startTimerFailedEventAttributes', 'historyEvent_startTimerFailedEventAttributes' - If the event is of type @StartTimerFailed@ then this member is set and
-- provides detailed information about the event. It isn\'t set for other
-- event types.
--
-- 'activityTaskScheduledEventAttributes', 'historyEvent_activityTaskScheduledEventAttributes' - If the event is of type @ActivityTaskScheduled@ then this member is set
-- and provides detailed information about the event. It isn\'t set for
-- other event types.
--
-- 'scheduleActivityTaskFailedEventAttributes', 'historyEvent_scheduleActivityTaskFailedEventAttributes' - If the event is of type @ScheduleActivityTaskFailed@ then this member is
-- set and provides detailed information about the event. It isn\'t set for
-- other event types.
--
-- 'childWorkflowExecutionCompletedEventAttributes', 'historyEvent_childWorkflowExecutionCompletedEventAttributes' - If the event is of type @ChildWorkflowExecutionCompleted@ then this
-- member is set and provides detailed information about the event. It
-- isn\'t set for other event types.
--
-- 'markerRecordedEventAttributes', 'historyEvent_markerRecordedEventAttributes' - If the event is of type @MarkerRecorded@ then this member is set and
-- provides detailed information about the event. It isn\'t set for other
-- event types.
--
-- 'scheduleLambdaFunctionFailedEventAttributes', 'historyEvent_scheduleLambdaFunctionFailedEventAttributes' - Provides the details of the @ScheduleLambdaFunctionFailed@ event. It
-- isn\'t set for other event types.
--
-- 'completeWorkflowExecutionFailedEventAttributes', 'historyEvent_completeWorkflowExecutionFailedEventAttributes' - If the event is of type @CompleteWorkflowExecutionFailed@ then this
-- member is set and provides detailed information about the event. It
-- isn\'t set for other event types.
--
-- 'lambdaFunctionCompletedEventAttributes', 'historyEvent_lambdaFunctionCompletedEventAttributes' - Provides the details of the @LambdaFunctionCompleted@ event. It isn\'t
-- set for other event types.
--
-- 'requestCancelExternalWorkflowExecutionFailedEventAttributes', 'historyEvent_requestCancelExternalWorkflowExecutionFailedEventAttributes' - If the event is of type @RequestCancelExternalWorkflowExecutionFailed@
-- then this member is set and provides detailed information about the
-- event. It isn\'t set for other event types.
--
-- 'timerCanceledEventAttributes', 'historyEvent_timerCanceledEventAttributes' - If the event is of type @TimerCanceled@ then this member is set and
-- provides detailed information about the event. It isn\'t set for other
-- event types.
--
-- 'workflowExecutionStartedEventAttributes', 'historyEvent_workflowExecutionStartedEventAttributes' - If the event is of type @WorkflowExecutionStarted@ then this member is
-- set and provides detailed information about the event. It isn\'t set for
-- other event types.
--
-- 'activityTaskCompletedEventAttributes', 'historyEvent_activityTaskCompletedEventAttributes' - If the event is of type @ActivityTaskCompleted@ then this member is set
-- and provides detailed information about the event. It isn\'t set for
-- other event types.
--
-- 'decisionTaskTimedOutEventAttributes', 'historyEvent_decisionTaskTimedOutEventAttributes' - If the event is of type @DecisionTaskTimedOut@ then this member is set
-- and provides detailed information about the event. It isn\'t set for
-- other event types.
--
-- 'cancelTimerFailedEventAttributes', 'historyEvent_cancelTimerFailedEventAttributes' - If the event is of type @CancelTimerFailed@ then this member is set and
-- provides detailed information about the event. It isn\'t set for other
-- event types.
--
-- 'childWorkflowExecutionStartedEventAttributes', 'historyEvent_childWorkflowExecutionStartedEventAttributes' - If the event is of type @ChildWorkflowExecutionStarted@ then this member
-- is set and provides detailed information about the event. It isn\'t set
-- for other event types.
--
-- 'activityTaskCanceledEventAttributes', 'historyEvent_activityTaskCanceledEventAttributes' - If the event is of type @ActivityTaskCanceled@ then this member is set
-- and provides detailed information about the event. It isn\'t set for
-- other event types.
--
-- 'activityTaskTimedOutEventAttributes', 'historyEvent_activityTaskTimedOutEventAttributes' - If the event is of type @ActivityTaskTimedOut@ then this member is set
-- and provides detailed information about the event. It isn\'t set for
-- other event types.
--
-- 'decisionTaskStartedEventAttributes', 'historyEvent_decisionTaskStartedEventAttributes' - If the event is of type @DecisionTaskStarted@ then this member is set
-- and provides detailed information about the event. It isn\'t set for
-- other event types.
--
-- 'workflowExecutionTerminatedEventAttributes', 'historyEvent_workflowExecutionTerminatedEventAttributes' - If the event is of type @WorkflowExecutionTerminated@ then this member
-- is set and provides detailed information about the event. It isn\'t set
-- for other event types.
--
-- 'childWorkflowExecutionCanceledEventAttributes', 'historyEvent_childWorkflowExecutionCanceledEventAttributes' - If the event is of type @ChildWorkflowExecutionCanceled@ then this
-- member is set and provides detailed information about the event. It
-- isn\'t set for other event types.
--
-- 'requestCancelActivityTaskFailedEventAttributes', 'historyEvent_requestCancelActivityTaskFailedEventAttributes' - If the event is of type @RequestCancelActivityTaskFailed@ then this
-- member is set and provides detailed information about the event. It
-- isn\'t set for other event types.
--
-- 'lambdaFunctionScheduledEventAttributes', 'historyEvent_lambdaFunctionScheduledEventAttributes' - Provides the details of the @LambdaFunctionScheduled@ event. It isn\'t
-- set for other event types.
--
-- 'childWorkflowExecutionTimedOutEventAttributes', 'historyEvent_childWorkflowExecutionTimedOutEventAttributes' - If the event is of type @ChildWorkflowExecutionTimedOut@ then this
-- member is set and provides detailed information about the event. It
-- isn\'t set for other event types.
--
-- 'cancelWorkflowExecutionFailedEventAttributes', 'historyEvent_cancelWorkflowExecutionFailedEventAttributes' - If the event is of type @CancelWorkflowExecutionFailed@ then this member
-- is set and provides detailed information about the event. It isn\'t set
-- for other event types.
--
-- 'startChildWorkflowExecutionInitiatedEventAttributes', 'historyEvent_startChildWorkflowExecutionInitiatedEventAttributes' - If the event is of type @StartChildWorkflowExecutionInitiated@ then this
-- member is set and provides detailed information about the event. It
-- isn\'t set for other event types.
--
-- 'signalExternalWorkflowExecutionFailedEventAttributes', 'historyEvent_signalExternalWorkflowExecutionFailedEventAttributes' - If the event is of type @SignalExternalWorkflowExecutionFailed@ then
-- this member is set and provides detailed information about the event. It
-- isn\'t set for other event types.
--
-- 'activityTaskStartedEventAttributes', 'historyEvent_activityTaskStartedEventAttributes' - If the event is of type @ActivityTaskStarted@ then this member is set
-- and provides detailed information about the event. It isn\'t set for
-- other event types.
--
-- 'startLambdaFunctionFailedEventAttributes', 'historyEvent_startLambdaFunctionFailedEventAttributes' - Provides the details of the @StartLambdaFunctionFailed@ event. It isn\'t
-- set for other event types.
--
-- 'childWorkflowExecutionTerminatedEventAttributes', 'historyEvent_childWorkflowExecutionTerminatedEventAttributes' - If the event is of type @ChildWorkflowExecutionTerminated@ then this
-- member is set and provides detailed information about the event. It
-- isn\'t set for other event types.
--
-- 'lambdaFunctionFailedEventAttributes', 'historyEvent_lambdaFunctionFailedEventAttributes' - Provides the details of the @LambdaFunctionFailed@ event. It isn\'t set
-- for other event types.
--
-- 'workflowExecutionCanceledEventAttributes', 'historyEvent_workflowExecutionCanceledEventAttributes' - If the event is of type @WorkflowExecutionCanceled@ then this member is
-- set and provides detailed information about the event. It isn\'t set for
-- other event types.
--
-- 'timerStartedEventAttributes', 'historyEvent_timerStartedEventAttributes' - If the event is of type @TimerStarted@ then this member is set and
-- provides detailed information about the event. It isn\'t set for other
-- event types.
--
-- 'activityTaskCancelRequestedEventAttributes', 'historyEvent_activityTaskCancelRequestedEventAttributes' - If the event is of type @ActivityTaskcancelRequested@ then this member
-- is set and provides detailed information about the event. It isn\'t set
-- for other event types.
--
-- 'workflowExecutionTimedOutEventAttributes', 'historyEvent_workflowExecutionTimedOutEventAttributes' - If the event is of type @WorkflowExecutionTimedOut@ then this member is
-- set and provides detailed information about the event. It isn\'t set for
-- other event types.
--
-- 'workflowExecutionSignaledEventAttributes', 'historyEvent_workflowExecutionSignaledEventAttributes' - If the event is of type @WorkflowExecutionSignaled@ then this member is
-- set and provides detailed information about the event. It isn\'t set for
-- other event types.
--
-- 'timerFiredEventAttributes', 'historyEvent_timerFiredEventAttributes' - If the event is of type @TimerFired@ then this member is set and
-- provides detailed information about the event. It isn\'t set for other
-- event types.
--
-- 'activityTaskFailedEventAttributes', 'historyEvent_activityTaskFailedEventAttributes' - If the event is of type @ActivityTaskFailed@ then this member is set and
-- provides detailed information about the event. It isn\'t set for other
-- event types.
--
-- 'externalWorkflowExecutionSignaledEventAttributes', 'historyEvent_externalWorkflowExecutionSignaledEventAttributes' - If the event is of type @ExternalWorkflowExecutionSignaled@ then this
-- member is set and provides detailed information about the event. It
-- isn\'t set for other event types.
--
-- 'decisionTaskCompletedEventAttributes', 'historyEvent_decisionTaskCompletedEventAttributes' - If the event is of type @DecisionTaskCompleted@ then this member is set
-- and provides detailed information about the event. It isn\'t set for
-- other event types.
--
-- 'startChildWorkflowExecutionFailedEventAttributes', 'historyEvent_startChildWorkflowExecutionFailedEventAttributes' - If the event is of type @StartChildWorkflowExecutionFailed@ then this
-- member is set and provides detailed information about the event. It
-- isn\'t set for other event types.
--
-- 'childWorkflowExecutionFailedEventAttributes', 'historyEvent_childWorkflowExecutionFailedEventAttributes' - If the event is of type @ChildWorkflowExecutionFailed@ then this member
-- is set and provides detailed information about the event. It isn\'t set
-- for other event types.
--
-- 'failWorkflowExecutionFailedEventAttributes', 'historyEvent_failWorkflowExecutionFailedEventAttributes' - If the event is of type @FailWorkflowExecutionFailed@ then this member
-- is set and provides detailed information about the event. It isn\'t set
-- for other event types.
--
-- 'continueAsNewWorkflowExecutionFailedEventAttributes', 'historyEvent_continueAsNewWorkflowExecutionFailedEventAttributes' - If the event is of type @ContinueAsNewWorkflowExecutionFailed@ then this
-- member is set and provides detailed information about the event. It
-- isn\'t set for other event types.
--
-- 'signalExternalWorkflowExecutionInitiatedEventAttributes', 'historyEvent_signalExternalWorkflowExecutionInitiatedEventAttributes' - If the event is of type @SignalExternalWorkflowExecutionInitiated@ then
-- this member is set and provides detailed information about the event. It
-- isn\'t set for other event types.
--
-- 'lambdaFunctionTimedOutEventAttributes', 'historyEvent_lambdaFunctionTimedOutEventAttributes' - Provides the details of the @LambdaFunctionTimedOut@ event. It isn\'t
-- set for other event types.
--
-- 'workflowExecutionFailedEventAttributes', 'historyEvent_workflowExecutionFailedEventAttributes' - If the event is of type @WorkflowExecutionFailed@ then this member is
-- set and provides detailed information about the event. It isn\'t set for
-- other event types.
--
-- 'workflowExecutionContinuedAsNewEventAttributes', 'historyEvent_workflowExecutionContinuedAsNewEventAttributes' - If the event is of type @WorkflowExecutionContinuedAsNew@ then this
-- member is set and provides detailed information about the event. It
-- isn\'t set for other event types.
--
-- 'externalWorkflowExecutionCancelRequestedEventAttributes', 'historyEvent_externalWorkflowExecutionCancelRequestedEventAttributes' - If the event is of type @ExternalWorkflowExecutionCancelRequested@ then
-- this member is set and provides detailed information about the event. It
-- isn\'t set for other event types.
--
-- 'eventTimestamp', 'historyEvent_eventTimestamp' - The date and time when the event occurred.
--
-- 'eventType', 'historyEvent_eventType' - The type of the history event.
--
-- 'eventId', 'historyEvent_eventId' - The system generated ID of the event. This ID uniquely identifies the
-- event with in the workflow execution history.
newHistoryEvent ::
  -- | 'eventTimestamp'
  Prelude.UTCTime ->
  -- | 'eventType'
  EventType ->
  -- | 'eventId'
  Prelude.Integer ->
  HistoryEvent
newHistoryEvent
  pEventTimestamp_
  pEventType_
  pEventId_ =
    HistoryEvent'
      { workflowExecutionCancelRequestedEventAttributes =
          Prelude.Nothing,
        recordMarkerFailedEventAttributes = Prelude.Nothing,
        requestCancelExternalWorkflowExecutionInitiatedEventAttributes =
          Prelude.Nothing,
        lambdaFunctionStartedEventAttributes =
          Prelude.Nothing,
        decisionTaskScheduledEventAttributes =
          Prelude.Nothing,
        workflowExecutionCompletedEventAttributes =
          Prelude.Nothing,
        startTimerFailedEventAttributes = Prelude.Nothing,
        activityTaskScheduledEventAttributes =
          Prelude.Nothing,
        scheduleActivityTaskFailedEventAttributes =
          Prelude.Nothing,
        childWorkflowExecutionCompletedEventAttributes =
          Prelude.Nothing,
        markerRecordedEventAttributes = Prelude.Nothing,
        scheduleLambdaFunctionFailedEventAttributes =
          Prelude.Nothing,
        completeWorkflowExecutionFailedEventAttributes =
          Prelude.Nothing,
        lambdaFunctionCompletedEventAttributes =
          Prelude.Nothing,
        requestCancelExternalWorkflowExecutionFailedEventAttributes =
          Prelude.Nothing,
        timerCanceledEventAttributes = Prelude.Nothing,
        workflowExecutionStartedEventAttributes =
          Prelude.Nothing,
        activityTaskCompletedEventAttributes =
          Prelude.Nothing,
        decisionTaskTimedOutEventAttributes =
          Prelude.Nothing,
        cancelTimerFailedEventAttributes = Prelude.Nothing,
        childWorkflowExecutionStartedEventAttributes =
          Prelude.Nothing,
        activityTaskCanceledEventAttributes =
          Prelude.Nothing,
        activityTaskTimedOutEventAttributes =
          Prelude.Nothing,
        decisionTaskStartedEventAttributes = Prelude.Nothing,
        workflowExecutionTerminatedEventAttributes =
          Prelude.Nothing,
        childWorkflowExecutionCanceledEventAttributes =
          Prelude.Nothing,
        requestCancelActivityTaskFailedEventAttributes =
          Prelude.Nothing,
        lambdaFunctionScheduledEventAttributes =
          Prelude.Nothing,
        childWorkflowExecutionTimedOutEventAttributes =
          Prelude.Nothing,
        cancelWorkflowExecutionFailedEventAttributes =
          Prelude.Nothing,
        startChildWorkflowExecutionInitiatedEventAttributes =
          Prelude.Nothing,
        signalExternalWorkflowExecutionFailedEventAttributes =
          Prelude.Nothing,
        activityTaskStartedEventAttributes = Prelude.Nothing,
        startLambdaFunctionFailedEventAttributes =
          Prelude.Nothing,
        childWorkflowExecutionTerminatedEventAttributes =
          Prelude.Nothing,
        lambdaFunctionFailedEventAttributes =
          Prelude.Nothing,
        workflowExecutionCanceledEventAttributes =
          Prelude.Nothing,
        timerStartedEventAttributes = Prelude.Nothing,
        activityTaskCancelRequestedEventAttributes =
          Prelude.Nothing,
        workflowExecutionTimedOutEventAttributes =
          Prelude.Nothing,
        workflowExecutionSignaledEventAttributes =
          Prelude.Nothing,
        timerFiredEventAttributes = Prelude.Nothing,
        activityTaskFailedEventAttributes = Prelude.Nothing,
        externalWorkflowExecutionSignaledEventAttributes =
          Prelude.Nothing,
        decisionTaskCompletedEventAttributes =
          Prelude.Nothing,
        startChildWorkflowExecutionFailedEventAttributes =
          Prelude.Nothing,
        childWorkflowExecutionFailedEventAttributes =
          Prelude.Nothing,
        failWorkflowExecutionFailedEventAttributes =
          Prelude.Nothing,
        continueAsNewWorkflowExecutionFailedEventAttributes =
          Prelude.Nothing,
        signalExternalWorkflowExecutionInitiatedEventAttributes =
          Prelude.Nothing,
        lambdaFunctionTimedOutEventAttributes =
          Prelude.Nothing,
        workflowExecutionFailedEventAttributes =
          Prelude.Nothing,
        workflowExecutionContinuedAsNewEventAttributes =
          Prelude.Nothing,
        externalWorkflowExecutionCancelRequestedEventAttributes =
          Prelude.Nothing,
        eventTimestamp = Core._Time Lens.# pEventTimestamp_,
        eventType = pEventType_,
        eventId = pEventId_
      }

-- | If the event is of type @WorkflowExecutionCancelRequested@ then this
-- member is set and provides detailed information about the event. It
-- isn\'t set for other event types.
historyEvent_workflowExecutionCancelRequestedEventAttributes :: Lens.Lens' HistoryEvent (Prelude.Maybe WorkflowExecutionCancelRequestedEventAttributes)
historyEvent_workflowExecutionCancelRequestedEventAttributes = Lens.lens (\HistoryEvent' {workflowExecutionCancelRequestedEventAttributes} -> workflowExecutionCancelRequestedEventAttributes) (\s@HistoryEvent' {} a -> s {workflowExecutionCancelRequestedEventAttributes = a} :: HistoryEvent)

-- | If the event is of type @DecisionTaskFailed@ then this member is set and
-- provides detailed information about the event. It isn\'t set for other
-- event types.
historyEvent_recordMarkerFailedEventAttributes :: Lens.Lens' HistoryEvent (Prelude.Maybe RecordMarkerFailedEventAttributes)
historyEvent_recordMarkerFailedEventAttributes = Lens.lens (\HistoryEvent' {recordMarkerFailedEventAttributes} -> recordMarkerFailedEventAttributes) (\s@HistoryEvent' {} a -> s {recordMarkerFailedEventAttributes = a} :: HistoryEvent)

-- | If the event is of type
-- @RequestCancelExternalWorkflowExecutionInitiated@ then this member is
-- set and provides detailed information about the event. It isn\'t set for
-- other event types.
historyEvent_requestCancelExternalWorkflowExecutionInitiatedEventAttributes :: Lens.Lens' HistoryEvent (Prelude.Maybe RequestCancelExternalWorkflowExecutionInitiatedEventAttributes)
historyEvent_requestCancelExternalWorkflowExecutionInitiatedEventAttributes = Lens.lens (\HistoryEvent' {requestCancelExternalWorkflowExecutionInitiatedEventAttributes} -> requestCancelExternalWorkflowExecutionInitiatedEventAttributes) (\s@HistoryEvent' {} a -> s {requestCancelExternalWorkflowExecutionInitiatedEventAttributes = a} :: HistoryEvent)

-- | Provides the details of the @LambdaFunctionStarted@ event. It isn\'t set
-- for other event types.
historyEvent_lambdaFunctionStartedEventAttributes :: Lens.Lens' HistoryEvent (Prelude.Maybe LambdaFunctionStartedEventAttributes)
historyEvent_lambdaFunctionStartedEventAttributes = Lens.lens (\HistoryEvent' {lambdaFunctionStartedEventAttributes} -> lambdaFunctionStartedEventAttributes) (\s@HistoryEvent' {} a -> s {lambdaFunctionStartedEventAttributes = a} :: HistoryEvent)

-- | If the event is of type @DecisionTaskScheduled@ then this member is set
-- and provides detailed information about the event. It isn\'t set for
-- other event types.
historyEvent_decisionTaskScheduledEventAttributes :: Lens.Lens' HistoryEvent (Prelude.Maybe DecisionTaskScheduledEventAttributes)
historyEvent_decisionTaskScheduledEventAttributes = Lens.lens (\HistoryEvent' {decisionTaskScheduledEventAttributes} -> decisionTaskScheduledEventAttributes) (\s@HistoryEvent' {} a -> s {decisionTaskScheduledEventAttributes = a} :: HistoryEvent)

-- | If the event is of type @WorkflowExecutionCompleted@ then this member is
-- set and provides detailed information about the event. It isn\'t set for
-- other event types.
historyEvent_workflowExecutionCompletedEventAttributes :: Lens.Lens' HistoryEvent (Prelude.Maybe WorkflowExecutionCompletedEventAttributes)
historyEvent_workflowExecutionCompletedEventAttributes = Lens.lens (\HistoryEvent' {workflowExecutionCompletedEventAttributes} -> workflowExecutionCompletedEventAttributes) (\s@HistoryEvent' {} a -> s {workflowExecutionCompletedEventAttributes = a} :: HistoryEvent)

-- | If the event is of type @StartTimerFailed@ then this member is set and
-- provides detailed information about the event. It isn\'t set for other
-- event types.
historyEvent_startTimerFailedEventAttributes :: Lens.Lens' HistoryEvent (Prelude.Maybe StartTimerFailedEventAttributes)
historyEvent_startTimerFailedEventAttributes = Lens.lens (\HistoryEvent' {startTimerFailedEventAttributes} -> startTimerFailedEventAttributes) (\s@HistoryEvent' {} a -> s {startTimerFailedEventAttributes = a} :: HistoryEvent)

-- | If the event is of type @ActivityTaskScheduled@ then this member is set
-- and provides detailed information about the event. It isn\'t set for
-- other event types.
historyEvent_activityTaskScheduledEventAttributes :: Lens.Lens' HistoryEvent (Prelude.Maybe ActivityTaskScheduledEventAttributes)
historyEvent_activityTaskScheduledEventAttributes = Lens.lens (\HistoryEvent' {activityTaskScheduledEventAttributes} -> activityTaskScheduledEventAttributes) (\s@HistoryEvent' {} a -> s {activityTaskScheduledEventAttributes = a} :: HistoryEvent)

-- | If the event is of type @ScheduleActivityTaskFailed@ then this member is
-- set and provides detailed information about the event. It isn\'t set for
-- other event types.
historyEvent_scheduleActivityTaskFailedEventAttributes :: Lens.Lens' HistoryEvent (Prelude.Maybe ScheduleActivityTaskFailedEventAttributes)
historyEvent_scheduleActivityTaskFailedEventAttributes = Lens.lens (\HistoryEvent' {scheduleActivityTaskFailedEventAttributes} -> scheduleActivityTaskFailedEventAttributes) (\s@HistoryEvent' {} a -> s {scheduleActivityTaskFailedEventAttributes = a} :: HistoryEvent)

-- | If the event is of type @ChildWorkflowExecutionCompleted@ then this
-- member is set and provides detailed information about the event. It
-- isn\'t set for other event types.
historyEvent_childWorkflowExecutionCompletedEventAttributes :: Lens.Lens' HistoryEvent (Prelude.Maybe ChildWorkflowExecutionCompletedEventAttributes)
historyEvent_childWorkflowExecutionCompletedEventAttributes = Lens.lens (\HistoryEvent' {childWorkflowExecutionCompletedEventAttributes} -> childWorkflowExecutionCompletedEventAttributes) (\s@HistoryEvent' {} a -> s {childWorkflowExecutionCompletedEventAttributes = a} :: HistoryEvent)

-- | If the event is of type @MarkerRecorded@ then this member is set and
-- provides detailed information about the event. It isn\'t set for other
-- event types.
historyEvent_markerRecordedEventAttributes :: Lens.Lens' HistoryEvent (Prelude.Maybe MarkerRecordedEventAttributes)
historyEvent_markerRecordedEventAttributes = Lens.lens (\HistoryEvent' {markerRecordedEventAttributes} -> markerRecordedEventAttributes) (\s@HistoryEvent' {} a -> s {markerRecordedEventAttributes = a} :: HistoryEvent)

-- | Provides the details of the @ScheduleLambdaFunctionFailed@ event. It
-- isn\'t set for other event types.
historyEvent_scheduleLambdaFunctionFailedEventAttributes :: Lens.Lens' HistoryEvent (Prelude.Maybe ScheduleLambdaFunctionFailedEventAttributes)
historyEvent_scheduleLambdaFunctionFailedEventAttributes = Lens.lens (\HistoryEvent' {scheduleLambdaFunctionFailedEventAttributes} -> scheduleLambdaFunctionFailedEventAttributes) (\s@HistoryEvent' {} a -> s {scheduleLambdaFunctionFailedEventAttributes = a} :: HistoryEvent)

-- | If the event is of type @CompleteWorkflowExecutionFailed@ then this
-- member is set and provides detailed information about the event. It
-- isn\'t set for other event types.
historyEvent_completeWorkflowExecutionFailedEventAttributes :: Lens.Lens' HistoryEvent (Prelude.Maybe CompleteWorkflowExecutionFailedEventAttributes)
historyEvent_completeWorkflowExecutionFailedEventAttributes = Lens.lens (\HistoryEvent' {completeWorkflowExecutionFailedEventAttributes} -> completeWorkflowExecutionFailedEventAttributes) (\s@HistoryEvent' {} a -> s {completeWorkflowExecutionFailedEventAttributes = a} :: HistoryEvent)

-- | Provides the details of the @LambdaFunctionCompleted@ event. It isn\'t
-- set for other event types.
historyEvent_lambdaFunctionCompletedEventAttributes :: Lens.Lens' HistoryEvent (Prelude.Maybe LambdaFunctionCompletedEventAttributes)
historyEvent_lambdaFunctionCompletedEventAttributes = Lens.lens (\HistoryEvent' {lambdaFunctionCompletedEventAttributes} -> lambdaFunctionCompletedEventAttributes) (\s@HistoryEvent' {} a -> s {lambdaFunctionCompletedEventAttributes = a} :: HistoryEvent)

-- | If the event is of type @RequestCancelExternalWorkflowExecutionFailed@
-- then this member is set and provides detailed information about the
-- event. It isn\'t set for other event types.
historyEvent_requestCancelExternalWorkflowExecutionFailedEventAttributes :: Lens.Lens' HistoryEvent (Prelude.Maybe RequestCancelExternalWorkflowExecutionFailedEventAttributes)
historyEvent_requestCancelExternalWorkflowExecutionFailedEventAttributes = Lens.lens (\HistoryEvent' {requestCancelExternalWorkflowExecutionFailedEventAttributes} -> requestCancelExternalWorkflowExecutionFailedEventAttributes) (\s@HistoryEvent' {} a -> s {requestCancelExternalWorkflowExecutionFailedEventAttributes = a} :: HistoryEvent)

-- | If the event is of type @TimerCanceled@ then this member is set and
-- provides detailed information about the event. It isn\'t set for other
-- event types.
historyEvent_timerCanceledEventAttributes :: Lens.Lens' HistoryEvent (Prelude.Maybe TimerCanceledEventAttributes)
historyEvent_timerCanceledEventAttributes = Lens.lens (\HistoryEvent' {timerCanceledEventAttributes} -> timerCanceledEventAttributes) (\s@HistoryEvent' {} a -> s {timerCanceledEventAttributes = a} :: HistoryEvent)

-- | If the event is of type @WorkflowExecutionStarted@ then this member is
-- set and provides detailed information about the event. It isn\'t set for
-- other event types.
historyEvent_workflowExecutionStartedEventAttributes :: Lens.Lens' HistoryEvent (Prelude.Maybe WorkflowExecutionStartedEventAttributes)
historyEvent_workflowExecutionStartedEventAttributes = Lens.lens (\HistoryEvent' {workflowExecutionStartedEventAttributes} -> workflowExecutionStartedEventAttributes) (\s@HistoryEvent' {} a -> s {workflowExecutionStartedEventAttributes = a} :: HistoryEvent)

-- | If the event is of type @ActivityTaskCompleted@ then this member is set
-- and provides detailed information about the event. It isn\'t set for
-- other event types.
historyEvent_activityTaskCompletedEventAttributes :: Lens.Lens' HistoryEvent (Prelude.Maybe ActivityTaskCompletedEventAttributes)
historyEvent_activityTaskCompletedEventAttributes = Lens.lens (\HistoryEvent' {activityTaskCompletedEventAttributes} -> activityTaskCompletedEventAttributes) (\s@HistoryEvent' {} a -> s {activityTaskCompletedEventAttributes = a} :: HistoryEvent)

-- | If the event is of type @DecisionTaskTimedOut@ then this member is set
-- and provides detailed information about the event. It isn\'t set for
-- other event types.
historyEvent_decisionTaskTimedOutEventAttributes :: Lens.Lens' HistoryEvent (Prelude.Maybe DecisionTaskTimedOutEventAttributes)
historyEvent_decisionTaskTimedOutEventAttributes = Lens.lens (\HistoryEvent' {decisionTaskTimedOutEventAttributes} -> decisionTaskTimedOutEventAttributes) (\s@HistoryEvent' {} a -> s {decisionTaskTimedOutEventAttributes = a} :: HistoryEvent)

-- | If the event is of type @CancelTimerFailed@ then this member is set and
-- provides detailed information about the event. It isn\'t set for other
-- event types.
historyEvent_cancelTimerFailedEventAttributes :: Lens.Lens' HistoryEvent (Prelude.Maybe CancelTimerFailedEventAttributes)
historyEvent_cancelTimerFailedEventAttributes = Lens.lens (\HistoryEvent' {cancelTimerFailedEventAttributes} -> cancelTimerFailedEventAttributes) (\s@HistoryEvent' {} a -> s {cancelTimerFailedEventAttributes = a} :: HistoryEvent)

-- | If the event is of type @ChildWorkflowExecutionStarted@ then this member
-- is set and provides detailed information about the event. It isn\'t set
-- for other event types.
historyEvent_childWorkflowExecutionStartedEventAttributes :: Lens.Lens' HistoryEvent (Prelude.Maybe ChildWorkflowExecutionStartedEventAttributes)
historyEvent_childWorkflowExecutionStartedEventAttributes = Lens.lens (\HistoryEvent' {childWorkflowExecutionStartedEventAttributes} -> childWorkflowExecutionStartedEventAttributes) (\s@HistoryEvent' {} a -> s {childWorkflowExecutionStartedEventAttributes = a} :: HistoryEvent)

-- | If the event is of type @ActivityTaskCanceled@ then this member is set
-- and provides detailed information about the event. It isn\'t set for
-- other event types.
historyEvent_activityTaskCanceledEventAttributes :: Lens.Lens' HistoryEvent (Prelude.Maybe ActivityTaskCanceledEventAttributes)
historyEvent_activityTaskCanceledEventAttributes = Lens.lens (\HistoryEvent' {activityTaskCanceledEventAttributes} -> activityTaskCanceledEventAttributes) (\s@HistoryEvent' {} a -> s {activityTaskCanceledEventAttributes = a} :: HistoryEvent)

-- | If the event is of type @ActivityTaskTimedOut@ then this member is set
-- and provides detailed information about the event. It isn\'t set for
-- other event types.
historyEvent_activityTaskTimedOutEventAttributes :: Lens.Lens' HistoryEvent (Prelude.Maybe ActivityTaskTimedOutEventAttributes)
historyEvent_activityTaskTimedOutEventAttributes = Lens.lens (\HistoryEvent' {activityTaskTimedOutEventAttributes} -> activityTaskTimedOutEventAttributes) (\s@HistoryEvent' {} a -> s {activityTaskTimedOutEventAttributes = a} :: HistoryEvent)

-- | If the event is of type @DecisionTaskStarted@ then this member is set
-- and provides detailed information about the event. It isn\'t set for
-- other event types.
historyEvent_decisionTaskStartedEventAttributes :: Lens.Lens' HistoryEvent (Prelude.Maybe DecisionTaskStartedEventAttributes)
historyEvent_decisionTaskStartedEventAttributes = Lens.lens (\HistoryEvent' {decisionTaskStartedEventAttributes} -> decisionTaskStartedEventAttributes) (\s@HistoryEvent' {} a -> s {decisionTaskStartedEventAttributes = a} :: HistoryEvent)

-- | If the event is of type @WorkflowExecutionTerminated@ then this member
-- is set and provides detailed information about the event. It isn\'t set
-- for other event types.
historyEvent_workflowExecutionTerminatedEventAttributes :: Lens.Lens' HistoryEvent (Prelude.Maybe WorkflowExecutionTerminatedEventAttributes)
historyEvent_workflowExecutionTerminatedEventAttributes = Lens.lens (\HistoryEvent' {workflowExecutionTerminatedEventAttributes} -> workflowExecutionTerminatedEventAttributes) (\s@HistoryEvent' {} a -> s {workflowExecutionTerminatedEventAttributes = a} :: HistoryEvent)

-- | If the event is of type @ChildWorkflowExecutionCanceled@ then this
-- member is set and provides detailed information about the event. It
-- isn\'t set for other event types.
historyEvent_childWorkflowExecutionCanceledEventAttributes :: Lens.Lens' HistoryEvent (Prelude.Maybe ChildWorkflowExecutionCanceledEventAttributes)
historyEvent_childWorkflowExecutionCanceledEventAttributes = Lens.lens (\HistoryEvent' {childWorkflowExecutionCanceledEventAttributes} -> childWorkflowExecutionCanceledEventAttributes) (\s@HistoryEvent' {} a -> s {childWorkflowExecutionCanceledEventAttributes = a} :: HistoryEvent)

-- | If the event is of type @RequestCancelActivityTaskFailed@ then this
-- member is set and provides detailed information about the event. It
-- isn\'t set for other event types.
historyEvent_requestCancelActivityTaskFailedEventAttributes :: Lens.Lens' HistoryEvent (Prelude.Maybe RequestCancelActivityTaskFailedEventAttributes)
historyEvent_requestCancelActivityTaskFailedEventAttributes = Lens.lens (\HistoryEvent' {requestCancelActivityTaskFailedEventAttributes} -> requestCancelActivityTaskFailedEventAttributes) (\s@HistoryEvent' {} a -> s {requestCancelActivityTaskFailedEventAttributes = a} :: HistoryEvent)

-- | Provides the details of the @LambdaFunctionScheduled@ event. It isn\'t
-- set for other event types.
historyEvent_lambdaFunctionScheduledEventAttributes :: Lens.Lens' HistoryEvent (Prelude.Maybe LambdaFunctionScheduledEventAttributes)
historyEvent_lambdaFunctionScheduledEventAttributes = Lens.lens (\HistoryEvent' {lambdaFunctionScheduledEventAttributes} -> lambdaFunctionScheduledEventAttributes) (\s@HistoryEvent' {} a -> s {lambdaFunctionScheduledEventAttributes = a} :: HistoryEvent)

-- | If the event is of type @ChildWorkflowExecutionTimedOut@ then this
-- member is set and provides detailed information about the event. It
-- isn\'t set for other event types.
historyEvent_childWorkflowExecutionTimedOutEventAttributes :: Lens.Lens' HistoryEvent (Prelude.Maybe ChildWorkflowExecutionTimedOutEventAttributes)
historyEvent_childWorkflowExecutionTimedOutEventAttributes = Lens.lens (\HistoryEvent' {childWorkflowExecutionTimedOutEventAttributes} -> childWorkflowExecutionTimedOutEventAttributes) (\s@HistoryEvent' {} a -> s {childWorkflowExecutionTimedOutEventAttributes = a} :: HistoryEvent)

-- | If the event is of type @CancelWorkflowExecutionFailed@ then this member
-- is set and provides detailed information about the event. It isn\'t set
-- for other event types.
historyEvent_cancelWorkflowExecutionFailedEventAttributes :: Lens.Lens' HistoryEvent (Prelude.Maybe CancelWorkflowExecutionFailedEventAttributes)
historyEvent_cancelWorkflowExecutionFailedEventAttributes = Lens.lens (\HistoryEvent' {cancelWorkflowExecutionFailedEventAttributes} -> cancelWorkflowExecutionFailedEventAttributes) (\s@HistoryEvent' {} a -> s {cancelWorkflowExecutionFailedEventAttributes = a} :: HistoryEvent)

-- | If the event is of type @StartChildWorkflowExecutionInitiated@ then this
-- member is set and provides detailed information about the event. It
-- isn\'t set for other event types.
historyEvent_startChildWorkflowExecutionInitiatedEventAttributes :: Lens.Lens' HistoryEvent (Prelude.Maybe StartChildWorkflowExecutionInitiatedEventAttributes)
historyEvent_startChildWorkflowExecutionInitiatedEventAttributes = Lens.lens (\HistoryEvent' {startChildWorkflowExecutionInitiatedEventAttributes} -> startChildWorkflowExecutionInitiatedEventAttributes) (\s@HistoryEvent' {} a -> s {startChildWorkflowExecutionInitiatedEventAttributes = a} :: HistoryEvent)

-- | If the event is of type @SignalExternalWorkflowExecutionFailed@ then
-- this member is set and provides detailed information about the event. It
-- isn\'t set for other event types.
historyEvent_signalExternalWorkflowExecutionFailedEventAttributes :: Lens.Lens' HistoryEvent (Prelude.Maybe SignalExternalWorkflowExecutionFailedEventAttributes)
historyEvent_signalExternalWorkflowExecutionFailedEventAttributes = Lens.lens (\HistoryEvent' {signalExternalWorkflowExecutionFailedEventAttributes} -> signalExternalWorkflowExecutionFailedEventAttributes) (\s@HistoryEvent' {} a -> s {signalExternalWorkflowExecutionFailedEventAttributes = a} :: HistoryEvent)

-- | If the event is of type @ActivityTaskStarted@ then this member is set
-- and provides detailed information about the event. It isn\'t set for
-- other event types.
historyEvent_activityTaskStartedEventAttributes :: Lens.Lens' HistoryEvent (Prelude.Maybe ActivityTaskStartedEventAttributes)
historyEvent_activityTaskStartedEventAttributes = Lens.lens (\HistoryEvent' {activityTaskStartedEventAttributes} -> activityTaskStartedEventAttributes) (\s@HistoryEvent' {} a -> s {activityTaskStartedEventAttributes = a} :: HistoryEvent)

-- | Provides the details of the @StartLambdaFunctionFailed@ event. It isn\'t
-- set for other event types.
historyEvent_startLambdaFunctionFailedEventAttributes :: Lens.Lens' HistoryEvent (Prelude.Maybe StartLambdaFunctionFailedEventAttributes)
historyEvent_startLambdaFunctionFailedEventAttributes = Lens.lens (\HistoryEvent' {startLambdaFunctionFailedEventAttributes} -> startLambdaFunctionFailedEventAttributes) (\s@HistoryEvent' {} a -> s {startLambdaFunctionFailedEventAttributes = a} :: HistoryEvent)

-- | If the event is of type @ChildWorkflowExecutionTerminated@ then this
-- member is set and provides detailed information about the event. It
-- isn\'t set for other event types.
historyEvent_childWorkflowExecutionTerminatedEventAttributes :: Lens.Lens' HistoryEvent (Prelude.Maybe ChildWorkflowExecutionTerminatedEventAttributes)
historyEvent_childWorkflowExecutionTerminatedEventAttributes = Lens.lens (\HistoryEvent' {childWorkflowExecutionTerminatedEventAttributes} -> childWorkflowExecutionTerminatedEventAttributes) (\s@HistoryEvent' {} a -> s {childWorkflowExecutionTerminatedEventAttributes = a} :: HistoryEvent)

-- | Provides the details of the @LambdaFunctionFailed@ event. It isn\'t set
-- for other event types.
historyEvent_lambdaFunctionFailedEventAttributes :: Lens.Lens' HistoryEvent (Prelude.Maybe LambdaFunctionFailedEventAttributes)
historyEvent_lambdaFunctionFailedEventAttributes = Lens.lens (\HistoryEvent' {lambdaFunctionFailedEventAttributes} -> lambdaFunctionFailedEventAttributes) (\s@HistoryEvent' {} a -> s {lambdaFunctionFailedEventAttributes = a} :: HistoryEvent)

-- | If the event is of type @WorkflowExecutionCanceled@ then this member is
-- set and provides detailed information about the event. It isn\'t set for
-- other event types.
historyEvent_workflowExecutionCanceledEventAttributes :: Lens.Lens' HistoryEvent (Prelude.Maybe WorkflowExecutionCanceledEventAttributes)
historyEvent_workflowExecutionCanceledEventAttributes = Lens.lens (\HistoryEvent' {workflowExecutionCanceledEventAttributes} -> workflowExecutionCanceledEventAttributes) (\s@HistoryEvent' {} a -> s {workflowExecutionCanceledEventAttributes = a} :: HistoryEvent)

-- | If the event is of type @TimerStarted@ then this member is set and
-- provides detailed information about the event. It isn\'t set for other
-- event types.
historyEvent_timerStartedEventAttributes :: Lens.Lens' HistoryEvent (Prelude.Maybe TimerStartedEventAttributes)
historyEvent_timerStartedEventAttributes = Lens.lens (\HistoryEvent' {timerStartedEventAttributes} -> timerStartedEventAttributes) (\s@HistoryEvent' {} a -> s {timerStartedEventAttributes = a} :: HistoryEvent)

-- | If the event is of type @ActivityTaskcancelRequested@ then this member
-- is set and provides detailed information about the event. It isn\'t set
-- for other event types.
historyEvent_activityTaskCancelRequestedEventAttributes :: Lens.Lens' HistoryEvent (Prelude.Maybe ActivityTaskCancelRequestedEventAttributes)
historyEvent_activityTaskCancelRequestedEventAttributes = Lens.lens (\HistoryEvent' {activityTaskCancelRequestedEventAttributes} -> activityTaskCancelRequestedEventAttributes) (\s@HistoryEvent' {} a -> s {activityTaskCancelRequestedEventAttributes = a} :: HistoryEvent)

-- | If the event is of type @WorkflowExecutionTimedOut@ then this member is
-- set and provides detailed information about the event. It isn\'t set for
-- other event types.
historyEvent_workflowExecutionTimedOutEventAttributes :: Lens.Lens' HistoryEvent (Prelude.Maybe WorkflowExecutionTimedOutEventAttributes)
historyEvent_workflowExecutionTimedOutEventAttributes = Lens.lens (\HistoryEvent' {workflowExecutionTimedOutEventAttributes} -> workflowExecutionTimedOutEventAttributes) (\s@HistoryEvent' {} a -> s {workflowExecutionTimedOutEventAttributes = a} :: HistoryEvent)

-- | If the event is of type @WorkflowExecutionSignaled@ then this member is
-- set and provides detailed information about the event. It isn\'t set for
-- other event types.
historyEvent_workflowExecutionSignaledEventAttributes :: Lens.Lens' HistoryEvent (Prelude.Maybe WorkflowExecutionSignaledEventAttributes)
historyEvent_workflowExecutionSignaledEventAttributes = Lens.lens (\HistoryEvent' {workflowExecutionSignaledEventAttributes} -> workflowExecutionSignaledEventAttributes) (\s@HistoryEvent' {} a -> s {workflowExecutionSignaledEventAttributes = a} :: HistoryEvent)

-- | If the event is of type @TimerFired@ then this member is set and
-- provides detailed information about the event. It isn\'t set for other
-- event types.
historyEvent_timerFiredEventAttributes :: Lens.Lens' HistoryEvent (Prelude.Maybe TimerFiredEventAttributes)
historyEvent_timerFiredEventAttributes = Lens.lens (\HistoryEvent' {timerFiredEventAttributes} -> timerFiredEventAttributes) (\s@HistoryEvent' {} a -> s {timerFiredEventAttributes = a} :: HistoryEvent)

-- | If the event is of type @ActivityTaskFailed@ then this member is set and
-- provides detailed information about the event. It isn\'t set for other
-- event types.
historyEvent_activityTaskFailedEventAttributes :: Lens.Lens' HistoryEvent (Prelude.Maybe ActivityTaskFailedEventAttributes)
historyEvent_activityTaskFailedEventAttributes = Lens.lens (\HistoryEvent' {activityTaskFailedEventAttributes} -> activityTaskFailedEventAttributes) (\s@HistoryEvent' {} a -> s {activityTaskFailedEventAttributes = a} :: HistoryEvent)

-- | If the event is of type @ExternalWorkflowExecutionSignaled@ then this
-- member is set and provides detailed information about the event. It
-- isn\'t set for other event types.
historyEvent_externalWorkflowExecutionSignaledEventAttributes :: Lens.Lens' HistoryEvent (Prelude.Maybe ExternalWorkflowExecutionSignaledEventAttributes)
historyEvent_externalWorkflowExecutionSignaledEventAttributes = Lens.lens (\HistoryEvent' {externalWorkflowExecutionSignaledEventAttributes} -> externalWorkflowExecutionSignaledEventAttributes) (\s@HistoryEvent' {} a -> s {externalWorkflowExecutionSignaledEventAttributes = a} :: HistoryEvent)

-- | If the event is of type @DecisionTaskCompleted@ then this member is set
-- and provides detailed information about the event. It isn\'t set for
-- other event types.
historyEvent_decisionTaskCompletedEventAttributes :: Lens.Lens' HistoryEvent (Prelude.Maybe DecisionTaskCompletedEventAttributes)
historyEvent_decisionTaskCompletedEventAttributes = Lens.lens (\HistoryEvent' {decisionTaskCompletedEventAttributes} -> decisionTaskCompletedEventAttributes) (\s@HistoryEvent' {} a -> s {decisionTaskCompletedEventAttributes = a} :: HistoryEvent)

-- | If the event is of type @StartChildWorkflowExecutionFailed@ then this
-- member is set and provides detailed information about the event. It
-- isn\'t set for other event types.
historyEvent_startChildWorkflowExecutionFailedEventAttributes :: Lens.Lens' HistoryEvent (Prelude.Maybe StartChildWorkflowExecutionFailedEventAttributes)
historyEvent_startChildWorkflowExecutionFailedEventAttributes = Lens.lens (\HistoryEvent' {startChildWorkflowExecutionFailedEventAttributes} -> startChildWorkflowExecutionFailedEventAttributes) (\s@HistoryEvent' {} a -> s {startChildWorkflowExecutionFailedEventAttributes = a} :: HistoryEvent)

-- | If the event is of type @ChildWorkflowExecutionFailed@ then this member
-- is set and provides detailed information about the event. It isn\'t set
-- for other event types.
historyEvent_childWorkflowExecutionFailedEventAttributes :: Lens.Lens' HistoryEvent (Prelude.Maybe ChildWorkflowExecutionFailedEventAttributes)
historyEvent_childWorkflowExecutionFailedEventAttributes = Lens.lens (\HistoryEvent' {childWorkflowExecutionFailedEventAttributes} -> childWorkflowExecutionFailedEventAttributes) (\s@HistoryEvent' {} a -> s {childWorkflowExecutionFailedEventAttributes = a} :: HistoryEvent)

-- | If the event is of type @FailWorkflowExecutionFailed@ then this member
-- is set and provides detailed information about the event. It isn\'t set
-- for other event types.
historyEvent_failWorkflowExecutionFailedEventAttributes :: Lens.Lens' HistoryEvent (Prelude.Maybe FailWorkflowExecutionFailedEventAttributes)
historyEvent_failWorkflowExecutionFailedEventAttributes = Lens.lens (\HistoryEvent' {failWorkflowExecutionFailedEventAttributes} -> failWorkflowExecutionFailedEventAttributes) (\s@HistoryEvent' {} a -> s {failWorkflowExecutionFailedEventAttributes = a} :: HistoryEvent)

-- | If the event is of type @ContinueAsNewWorkflowExecutionFailed@ then this
-- member is set and provides detailed information about the event. It
-- isn\'t set for other event types.
historyEvent_continueAsNewWorkflowExecutionFailedEventAttributes :: Lens.Lens' HistoryEvent (Prelude.Maybe ContinueAsNewWorkflowExecutionFailedEventAttributes)
historyEvent_continueAsNewWorkflowExecutionFailedEventAttributes = Lens.lens (\HistoryEvent' {continueAsNewWorkflowExecutionFailedEventAttributes} -> continueAsNewWorkflowExecutionFailedEventAttributes) (\s@HistoryEvent' {} a -> s {continueAsNewWorkflowExecutionFailedEventAttributes = a} :: HistoryEvent)

-- | If the event is of type @SignalExternalWorkflowExecutionInitiated@ then
-- this member is set and provides detailed information about the event. It
-- isn\'t set for other event types.
historyEvent_signalExternalWorkflowExecutionInitiatedEventAttributes :: Lens.Lens' HistoryEvent (Prelude.Maybe SignalExternalWorkflowExecutionInitiatedEventAttributes)
historyEvent_signalExternalWorkflowExecutionInitiatedEventAttributes = Lens.lens (\HistoryEvent' {signalExternalWorkflowExecutionInitiatedEventAttributes} -> signalExternalWorkflowExecutionInitiatedEventAttributes) (\s@HistoryEvent' {} a -> s {signalExternalWorkflowExecutionInitiatedEventAttributes = a} :: HistoryEvent)

-- | Provides the details of the @LambdaFunctionTimedOut@ event. It isn\'t
-- set for other event types.
historyEvent_lambdaFunctionTimedOutEventAttributes :: Lens.Lens' HistoryEvent (Prelude.Maybe LambdaFunctionTimedOutEventAttributes)
historyEvent_lambdaFunctionTimedOutEventAttributes = Lens.lens (\HistoryEvent' {lambdaFunctionTimedOutEventAttributes} -> lambdaFunctionTimedOutEventAttributes) (\s@HistoryEvent' {} a -> s {lambdaFunctionTimedOutEventAttributes = a} :: HistoryEvent)

-- | If the event is of type @WorkflowExecutionFailed@ then this member is
-- set and provides detailed information about the event. It isn\'t set for
-- other event types.
historyEvent_workflowExecutionFailedEventAttributes :: Lens.Lens' HistoryEvent (Prelude.Maybe WorkflowExecutionFailedEventAttributes)
historyEvent_workflowExecutionFailedEventAttributes = Lens.lens (\HistoryEvent' {workflowExecutionFailedEventAttributes} -> workflowExecutionFailedEventAttributes) (\s@HistoryEvent' {} a -> s {workflowExecutionFailedEventAttributes = a} :: HistoryEvent)

-- | If the event is of type @WorkflowExecutionContinuedAsNew@ then this
-- member is set and provides detailed information about the event. It
-- isn\'t set for other event types.
historyEvent_workflowExecutionContinuedAsNewEventAttributes :: Lens.Lens' HistoryEvent (Prelude.Maybe WorkflowExecutionContinuedAsNewEventAttributes)
historyEvent_workflowExecutionContinuedAsNewEventAttributes = Lens.lens (\HistoryEvent' {workflowExecutionContinuedAsNewEventAttributes} -> workflowExecutionContinuedAsNewEventAttributes) (\s@HistoryEvent' {} a -> s {workflowExecutionContinuedAsNewEventAttributes = a} :: HistoryEvent)

-- | If the event is of type @ExternalWorkflowExecutionCancelRequested@ then
-- this member is set and provides detailed information about the event. It
-- isn\'t set for other event types.
historyEvent_externalWorkflowExecutionCancelRequestedEventAttributes :: Lens.Lens' HistoryEvent (Prelude.Maybe ExternalWorkflowExecutionCancelRequestedEventAttributes)
historyEvent_externalWorkflowExecutionCancelRequestedEventAttributes = Lens.lens (\HistoryEvent' {externalWorkflowExecutionCancelRequestedEventAttributes} -> externalWorkflowExecutionCancelRequestedEventAttributes) (\s@HistoryEvent' {} a -> s {externalWorkflowExecutionCancelRequestedEventAttributes = a} :: HistoryEvent)

-- | The date and time when the event occurred.
historyEvent_eventTimestamp :: Lens.Lens' HistoryEvent Prelude.UTCTime
historyEvent_eventTimestamp = Lens.lens (\HistoryEvent' {eventTimestamp} -> eventTimestamp) (\s@HistoryEvent' {} a -> s {eventTimestamp = a} :: HistoryEvent) Prelude.. Core._Time

-- | The type of the history event.
historyEvent_eventType :: Lens.Lens' HistoryEvent EventType
historyEvent_eventType = Lens.lens (\HistoryEvent' {eventType} -> eventType) (\s@HistoryEvent' {} a -> s {eventType = a} :: HistoryEvent)

-- | The system generated ID of the event. This ID uniquely identifies the
-- event with in the workflow execution history.
historyEvent_eventId :: Lens.Lens' HistoryEvent Prelude.Integer
historyEvent_eventId = Lens.lens (\HistoryEvent' {eventId} -> eventId) (\s@HistoryEvent' {} a -> s {eventId = a} :: HistoryEvent)

instance Core.FromJSON HistoryEvent where
  parseJSON =
    Core.withObject
      "HistoryEvent"
      ( \x ->
          HistoryEvent'
            Prelude.<$> ( x
                            Core..:? "workflowExecutionCancelRequestedEventAttributes"
                        )
            Prelude.<*> (x Core..:? "recordMarkerFailedEventAttributes")
            Prelude.<*> ( x
                            Core..:? "requestCancelExternalWorkflowExecutionInitiatedEventAttributes"
                        )
            Prelude.<*> (x Core..:? "lambdaFunctionStartedEventAttributes")
            Prelude.<*> (x Core..:? "decisionTaskScheduledEventAttributes")
            Prelude.<*> ( x
                            Core..:? "workflowExecutionCompletedEventAttributes"
                        )
            Prelude.<*> (x Core..:? "startTimerFailedEventAttributes")
            Prelude.<*> (x Core..:? "activityTaskScheduledEventAttributes")
            Prelude.<*> ( x
                            Core..:? "scheduleActivityTaskFailedEventAttributes"
                        )
            Prelude.<*> ( x
                            Core..:? "childWorkflowExecutionCompletedEventAttributes"
                        )
            Prelude.<*> (x Core..:? "markerRecordedEventAttributes")
            Prelude.<*> ( x
                            Core..:? "scheduleLambdaFunctionFailedEventAttributes"
                        )
            Prelude.<*> ( x
                            Core..:? "completeWorkflowExecutionFailedEventAttributes"
                        )
            Prelude.<*> (x Core..:? "lambdaFunctionCompletedEventAttributes")
            Prelude.<*> ( x
                            Core..:? "requestCancelExternalWorkflowExecutionFailedEventAttributes"
                        )
            Prelude.<*> (x Core..:? "timerCanceledEventAttributes")
            Prelude.<*> ( x
                            Core..:? "workflowExecutionStartedEventAttributes"
                        )
            Prelude.<*> (x Core..:? "activityTaskCompletedEventAttributes")
            Prelude.<*> (x Core..:? "decisionTaskTimedOutEventAttributes")
            Prelude.<*> (x Core..:? "cancelTimerFailedEventAttributes")
            Prelude.<*> ( x
                            Core..:? "childWorkflowExecutionStartedEventAttributes"
                        )
            Prelude.<*> (x Core..:? "activityTaskCanceledEventAttributes")
            Prelude.<*> (x Core..:? "activityTaskTimedOutEventAttributes")
            Prelude.<*> (x Core..:? "decisionTaskStartedEventAttributes")
            Prelude.<*> ( x
                            Core..:? "workflowExecutionTerminatedEventAttributes"
                        )
            Prelude.<*> ( x
                            Core..:? "childWorkflowExecutionCanceledEventAttributes"
                        )
            Prelude.<*> ( x
                            Core..:? "requestCancelActivityTaskFailedEventAttributes"
                        )
            Prelude.<*> (x Core..:? "lambdaFunctionScheduledEventAttributes")
            Prelude.<*> ( x
                            Core..:? "childWorkflowExecutionTimedOutEventAttributes"
                        )
            Prelude.<*> ( x
                            Core..:? "cancelWorkflowExecutionFailedEventAttributes"
                        )
            Prelude.<*> ( x
                            Core..:? "startChildWorkflowExecutionInitiatedEventAttributes"
                        )
            Prelude.<*> ( x
                            Core..:? "signalExternalWorkflowExecutionFailedEventAttributes"
                        )
            Prelude.<*> (x Core..:? "activityTaskStartedEventAttributes")
            Prelude.<*> ( x
                            Core..:? "startLambdaFunctionFailedEventAttributes"
                        )
            Prelude.<*> ( x
                            Core..:? "childWorkflowExecutionTerminatedEventAttributes"
                        )
            Prelude.<*> (x Core..:? "lambdaFunctionFailedEventAttributes")
            Prelude.<*> ( x
                            Core..:? "workflowExecutionCanceledEventAttributes"
                        )
            Prelude.<*> (x Core..:? "timerStartedEventAttributes")
            Prelude.<*> ( x
                            Core..:? "activityTaskCancelRequestedEventAttributes"
                        )
            Prelude.<*> ( x
                            Core..:? "workflowExecutionTimedOutEventAttributes"
                        )
            Prelude.<*> ( x
                            Core..:? "workflowExecutionSignaledEventAttributes"
                        )
            Prelude.<*> (x Core..:? "timerFiredEventAttributes")
            Prelude.<*> (x Core..:? "activityTaskFailedEventAttributes")
            Prelude.<*> ( x
                            Core..:? "externalWorkflowExecutionSignaledEventAttributes"
                        )
            Prelude.<*> (x Core..:? "decisionTaskCompletedEventAttributes")
            Prelude.<*> ( x
                            Core..:? "startChildWorkflowExecutionFailedEventAttributes"
                        )
            Prelude.<*> ( x
                            Core..:? "childWorkflowExecutionFailedEventAttributes"
                        )
            Prelude.<*> ( x
                            Core..:? "failWorkflowExecutionFailedEventAttributes"
                        )
            Prelude.<*> ( x
                            Core..:? "continueAsNewWorkflowExecutionFailedEventAttributes"
                        )
            Prelude.<*> ( x
                            Core..:? "signalExternalWorkflowExecutionInitiatedEventAttributes"
                        )
            Prelude.<*> (x Core..:? "lambdaFunctionTimedOutEventAttributes")
            Prelude.<*> (x Core..:? "workflowExecutionFailedEventAttributes")
            Prelude.<*> ( x
                            Core..:? "workflowExecutionContinuedAsNewEventAttributes"
                        )
            Prelude.<*> ( x
                            Core..:? "externalWorkflowExecutionCancelRequestedEventAttributes"
                        )
            Prelude.<*> (x Core..: "eventTimestamp")
            Prelude.<*> (x Core..: "eventType")
            Prelude.<*> (x Core..: "eventId")
      )

instance Prelude.Hashable HistoryEvent where
  hashWithSalt salt' HistoryEvent' {..} =
    salt' `Prelude.hashWithSalt` eventId
      `Prelude.hashWithSalt` eventType
      `Prelude.hashWithSalt` eventTimestamp
      `Prelude.hashWithSalt` externalWorkflowExecutionCancelRequestedEventAttributes
      `Prelude.hashWithSalt` workflowExecutionContinuedAsNewEventAttributes
      `Prelude.hashWithSalt` workflowExecutionFailedEventAttributes
      `Prelude.hashWithSalt` lambdaFunctionTimedOutEventAttributes
      `Prelude.hashWithSalt` signalExternalWorkflowExecutionInitiatedEventAttributes
      `Prelude.hashWithSalt` continueAsNewWorkflowExecutionFailedEventAttributes
      `Prelude.hashWithSalt` failWorkflowExecutionFailedEventAttributes
      `Prelude.hashWithSalt` childWorkflowExecutionFailedEventAttributes
      `Prelude.hashWithSalt` startChildWorkflowExecutionFailedEventAttributes
      `Prelude.hashWithSalt` decisionTaskCompletedEventAttributes
      `Prelude.hashWithSalt` externalWorkflowExecutionSignaledEventAttributes
      `Prelude.hashWithSalt` activityTaskFailedEventAttributes
      `Prelude.hashWithSalt` timerFiredEventAttributes
      `Prelude.hashWithSalt` workflowExecutionSignaledEventAttributes
      `Prelude.hashWithSalt` workflowExecutionTimedOutEventAttributes
      `Prelude.hashWithSalt` activityTaskCancelRequestedEventAttributes
      `Prelude.hashWithSalt` timerStartedEventAttributes
      `Prelude.hashWithSalt` workflowExecutionCanceledEventAttributes
      `Prelude.hashWithSalt` lambdaFunctionFailedEventAttributes
      `Prelude.hashWithSalt` childWorkflowExecutionTerminatedEventAttributes
      `Prelude.hashWithSalt` startLambdaFunctionFailedEventAttributes
      `Prelude.hashWithSalt` activityTaskStartedEventAttributes
      `Prelude.hashWithSalt` signalExternalWorkflowExecutionFailedEventAttributes
      `Prelude.hashWithSalt` startChildWorkflowExecutionInitiatedEventAttributes
      `Prelude.hashWithSalt` cancelWorkflowExecutionFailedEventAttributes
      `Prelude.hashWithSalt` childWorkflowExecutionTimedOutEventAttributes
      `Prelude.hashWithSalt` lambdaFunctionScheduledEventAttributes
      `Prelude.hashWithSalt` requestCancelActivityTaskFailedEventAttributes
      `Prelude.hashWithSalt` childWorkflowExecutionCanceledEventAttributes
      `Prelude.hashWithSalt` workflowExecutionTerminatedEventAttributes
      `Prelude.hashWithSalt` decisionTaskStartedEventAttributes
      `Prelude.hashWithSalt` activityTaskTimedOutEventAttributes
      `Prelude.hashWithSalt` activityTaskCanceledEventAttributes
      `Prelude.hashWithSalt` childWorkflowExecutionStartedEventAttributes
      `Prelude.hashWithSalt` cancelTimerFailedEventAttributes
      `Prelude.hashWithSalt` decisionTaskTimedOutEventAttributes
      `Prelude.hashWithSalt` activityTaskCompletedEventAttributes
      `Prelude.hashWithSalt` workflowExecutionStartedEventAttributes
      `Prelude.hashWithSalt` timerCanceledEventAttributes
      `Prelude.hashWithSalt` requestCancelExternalWorkflowExecutionFailedEventAttributes
      `Prelude.hashWithSalt` lambdaFunctionCompletedEventAttributes
      `Prelude.hashWithSalt` completeWorkflowExecutionFailedEventAttributes
      `Prelude.hashWithSalt` scheduleLambdaFunctionFailedEventAttributes
      `Prelude.hashWithSalt` markerRecordedEventAttributes
      `Prelude.hashWithSalt` childWorkflowExecutionCompletedEventAttributes
      `Prelude.hashWithSalt` scheduleActivityTaskFailedEventAttributes
      `Prelude.hashWithSalt` activityTaskScheduledEventAttributes
      `Prelude.hashWithSalt` startTimerFailedEventAttributes
      `Prelude.hashWithSalt` workflowExecutionCompletedEventAttributes
      `Prelude.hashWithSalt` decisionTaskScheduledEventAttributes
      `Prelude.hashWithSalt` lambdaFunctionStartedEventAttributes
      `Prelude.hashWithSalt` requestCancelExternalWorkflowExecutionInitiatedEventAttributes
      `Prelude.hashWithSalt` recordMarkerFailedEventAttributes
      `Prelude.hashWithSalt` workflowExecutionCancelRequestedEventAttributes

instance Prelude.NFData HistoryEvent where
  rnf HistoryEvent' {..} =
    Prelude.rnf
      workflowExecutionCancelRequestedEventAttributes
      `Prelude.seq` Prelude.rnf eventId
      `Prelude.seq` Prelude.rnf eventType
      `Prelude.seq` Prelude.rnf eventTimestamp
      `Prelude.seq` Prelude.rnf
        externalWorkflowExecutionCancelRequestedEventAttributes
      `Prelude.seq` Prelude.rnf
        workflowExecutionContinuedAsNewEventAttributes
      `Prelude.seq` Prelude.rnf workflowExecutionFailedEventAttributes
      `Prelude.seq` Prelude.rnf lambdaFunctionTimedOutEventAttributes
      `Prelude.seq` Prelude.rnf
        signalExternalWorkflowExecutionInitiatedEventAttributes
      `Prelude.seq` Prelude.rnf
        continueAsNewWorkflowExecutionFailedEventAttributes
      `Prelude.seq` Prelude.rnf
        failWorkflowExecutionFailedEventAttributes
      `Prelude.seq` Prelude.rnf
        childWorkflowExecutionFailedEventAttributes
      `Prelude.seq` Prelude.rnf
        startChildWorkflowExecutionFailedEventAttributes
      `Prelude.seq` Prelude.rnf decisionTaskCompletedEventAttributes
      `Prelude.seq` Prelude.rnf
        externalWorkflowExecutionSignaledEventAttributes
      `Prelude.seq` Prelude.rnf activityTaskFailedEventAttributes
      `Prelude.seq` Prelude.rnf timerFiredEventAttributes
      `Prelude.seq` Prelude.rnf workflowExecutionSignaledEventAttributes
      `Prelude.seq` Prelude.rnf workflowExecutionTimedOutEventAttributes
      `Prelude.seq` Prelude.rnf
        activityTaskCancelRequestedEventAttributes
      `Prelude.seq` Prelude.rnf timerStartedEventAttributes
      `Prelude.seq` Prelude.rnf workflowExecutionCanceledEventAttributes
      `Prelude.seq` Prelude.rnf lambdaFunctionFailedEventAttributes
      `Prelude.seq` Prelude.rnf
        childWorkflowExecutionTerminatedEventAttributes
      `Prelude.seq` Prelude.rnf startLambdaFunctionFailedEventAttributes
      `Prelude.seq` Prelude.rnf activityTaskStartedEventAttributes
      `Prelude.seq` Prelude.rnf
        signalExternalWorkflowExecutionFailedEventAttributes
      `Prelude.seq` Prelude.rnf
        startChildWorkflowExecutionInitiatedEventAttributes
      `Prelude.seq` Prelude.rnf
        cancelWorkflowExecutionFailedEventAttributes
      `Prelude.seq` Prelude.rnf
        childWorkflowExecutionTimedOutEventAttributes
      `Prelude.seq` Prelude.rnf lambdaFunctionScheduledEventAttributes
      `Prelude.seq` Prelude.rnf
        requestCancelActivityTaskFailedEventAttributes
      `Prelude.seq` Prelude.rnf
        childWorkflowExecutionCanceledEventAttributes
      `Prelude.seq` Prelude.rnf
        workflowExecutionTerminatedEventAttributes
      `Prelude.seq` Prelude.rnf decisionTaskStartedEventAttributes
      `Prelude.seq` Prelude.rnf activityTaskTimedOutEventAttributes
      `Prelude.seq` Prelude.rnf activityTaskCanceledEventAttributes
      `Prelude.seq` Prelude.rnf
        childWorkflowExecutionStartedEventAttributes
      `Prelude.seq` Prelude.rnf cancelTimerFailedEventAttributes
      `Prelude.seq` Prelude.rnf decisionTaskTimedOutEventAttributes
      `Prelude.seq` Prelude.rnf activityTaskCompletedEventAttributes
      `Prelude.seq` Prelude.rnf workflowExecutionStartedEventAttributes
      `Prelude.seq` Prelude.rnf timerCanceledEventAttributes
      `Prelude.seq` Prelude.rnf
        requestCancelExternalWorkflowExecutionFailedEventAttributes
      `Prelude.seq` Prelude.rnf lambdaFunctionCompletedEventAttributes
      `Prelude.seq` Prelude.rnf
        completeWorkflowExecutionFailedEventAttributes
      `Prelude.seq` Prelude.rnf
        scheduleLambdaFunctionFailedEventAttributes
      `Prelude.seq` Prelude.rnf markerRecordedEventAttributes
      `Prelude.seq` Prelude.rnf
        childWorkflowExecutionCompletedEventAttributes
      `Prelude.seq` Prelude.rnf scheduleActivityTaskFailedEventAttributes
      `Prelude.seq` Prelude.rnf activityTaskScheduledEventAttributes
      `Prelude.seq` Prelude.rnf startTimerFailedEventAttributes
      `Prelude.seq` Prelude.rnf workflowExecutionCompletedEventAttributes
      `Prelude.seq` Prelude.rnf decisionTaskScheduledEventAttributes
      `Prelude.seq` Prelude.rnf lambdaFunctionStartedEventAttributes
      `Prelude.seq` Prelude.rnf
        requestCancelExternalWorkflowExecutionInitiatedEventAttributes
      `Prelude.seq` Prelude.rnf recordMarkerFailedEventAttributes
