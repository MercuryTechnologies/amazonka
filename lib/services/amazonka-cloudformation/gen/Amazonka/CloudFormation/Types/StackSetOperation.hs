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
-- Module      : Amazonka.CloudFormation.Types.StackSetOperation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFormation.Types.StackSetOperation where

import Amazonka.CloudFormation.Types.DeploymentTargets
import Amazonka.CloudFormation.Types.StackSetDriftDetectionDetails
import Amazonka.CloudFormation.Types.StackSetOperationAction
import Amazonka.CloudFormation.Types.StackSetOperationPreferences
import Amazonka.CloudFormation.Types.StackSetOperationStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The structure that contains information about a stack set operation.
--
-- /See:/ 'newStackSetOperation' smart constructor.
data StackSetOperation = StackSetOperation'
  { -- | Detailed information about the drift status of the stack set. This
    -- includes information about drift operations currently being performed on
    -- the stack set.
    --
    -- this information will only be present for stack set operations whose
    -- @Action@ type is @DETECT_DRIFT@.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-drift.html Detecting Unmanaged Changes in Stack Sets>
    -- in the CloudFormation User Guide.
    stackSetDriftDetectionDetails :: Prelude.Maybe StackSetDriftDetectionDetails,
    -- | The status of the operation.
    --
    -- -   @FAILED@: The operation exceeded the specified failure tolerance.
    --     The failure tolerance value that you\'ve set for an operation is
    --     applied for each Region during stack create and update operations.
    --     If the number of failed stacks within a Region exceeds the failure
    --     tolerance, the status of the operation in the Region is set to
    --     @FAILED@. This in turn sets the status of the operation as a whole
    --     to @FAILED@, and CloudFormation cancels the operation in any
    --     remaining Regions.
    --
    -- -   @QUEUED@: [Service-managed permissions] For automatic deployments
    --     that require a sequence of operations, the operation is queued to be
    --     performed. For more information, see the
    --     <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-concepts.html#stackset-status-codes stack set operation status codes>
    --     in the CloudFormation User Guide.
    --
    -- -   @RUNNING@: The operation is currently being performed.
    --
    -- -   @STOPPED@: The user has cancelled the operation.
    --
    -- -   @STOPPING@: The operation is in the process of stopping, at user
    --     request.
    --
    -- -   @SUCCEEDED@: The operation completed creating or updating all the
    --     specified stacks without exceeding the failure tolerance for the
    --     operation.
    status :: Prelude.Maybe StackSetOperationStatus,
    -- | The Amazon Resource Number (ARN) of the IAM role used to perform this
    -- stack set operation.
    --
    -- Use customized administrator roles to control which users or groups can
    -- manage specific stack sets within the same administrator account. For
    -- more information, see
    -- <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-prereqs.html Define Permissions for Multiple Administrators>
    -- in the /CloudFormation User Guide/.
    administrationRoleARN :: Prelude.Maybe Prelude.Text,
    -- | The type of stack set operation: @CREATE@, @UPDATE@, or @DELETE@. Create
    -- and delete operations affect only the specified stack set instances that
    -- are associated with the specified stack set. Update operations affect
    -- both the stack set itself, as well as /all/ associated stack set
    -- instances.
    action :: Prelude.Maybe StackSetOperationAction,
    -- | The time at which the stack set operation ended, across all accounts and
    -- Regions specified. Note that this doesn\'t necessarily mean that the
    -- stack set operation was successful, or even attempted, in each account
    -- or Region.
    endTimestamp :: Prelude.Maybe Core.ISO8601,
    -- | The time at which the operation was initiated. Note that the creation
    -- times for the stack set operation might differ from the creation time of
    -- the individual stacks themselves. This is because CloudFormation needs
    -- to perform preparatory work for the operation, such as dispatching the
    -- work to the requested Regions, before actually creating the first
    -- stacks.
    creationTimestamp :: Prelude.Maybe Core.ISO8601,
    -- | The preferences for how CloudFormation performs this stack set
    -- operation.
    operationPreferences :: Prelude.Maybe StackSetOperationPreferences,
    -- | The unique ID of a stack set operation.
    operationId :: Prelude.Maybe Prelude.Text,
    -- | For stack set operations of action type @DELETE@, specifies whether to
    -- remove the stack instances from the specified stack set, but doesn\'t
    -- delete the stacks. You can\'t reassociate a retained stack, or add an
    -- existing, saved stack to a new stack set.
    retainStacks :: Prelude.Maybe Prelude.Bool,
    -- | [Service-managed permissions] The Organizations accounts affected by the
    -- stack operation.
    deploymentTargets :: Prelude.Maybe DeploymentTargets,
    -- | The ID of the stack set.
    stackSetId :: Prelude.Maybe Prelude.Text,
    -- | The name of the IAM execution role used to create or update the stack
    -- set.
    --
    -- Use customized execution roles to control which stack resources users
    -- and groups can include in their stack sets.
    executionRoleName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StackSetOperation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stackSetDriftDetectionDetails', 'stackSetOperation_stackSetDriftDetectionDetails' - Detailed information about the drift status of the stack set. This
-- includes information about drift operations currently being performed on
-- the stack set.
--
-- this information will only be present for stack set operations whose
-- @Action@ type is @DETECT_DRIFT@.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-drift.html Detecting Unmanaged Changes in Stack Sets>
-- in the CloudFormation User Guide.
--
-- 'status', 'stackSetOperation_status' - The status of the operation.
--
-- -   @FAILED@: The operation exceeded the specified failure tolerance.
--     The failure tolerance value that you\'ve set for an operation is
--     applied for each Region during stack create and update operations.
--     If the number of failed stacks within a Region exceeds the failure
--     tolerance, the status of the operation in the Region is set to
--     @FAILED@. This in turn sets the status of the operation as a whole
--     to @FAILED@, and CloudFormation cancels the operation in any
--     remaining Regions.
--
-- -   @QUEUED@: [Service-managed permissions] For automatic deployments
--     that require a sequence of operations, the operation is queued to be
--     performed. For more information, see the
--     <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-concepts.html#stackset-status-codes stack set operation status codes>
--     in the CloudFormation User Guide.
--
-- -   @RUNNING@: The operation is currently being performed.
--
-- -   @STOPPED@: The user has cancelled the operation.
--
-- -   @STOPPING@: The operation is in the process of stopping, at user
--     request.
--
-- -   @SUCCEEDED@: The operation completed creating or updating all the
--     specified stacks without exceeding the failure tolerance for the
--     operation.
--
-- 'administrationRoleARN', 'stackSetOperation_administrationRoleARN' - The Amazon Resource Number (ARN) of the IAM role used to perform this
-- stack set operation.
--
-- Use customized administrator roles to control which users or groups can
-- manage specific stack sets within the same administrator account. For
-- more information, see
-- <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-prereqs.html Define Permissions for Multiple Administrators>
-- in the /CloudFormation User Guide/.
--
-- 'action', 'stackSetOperation_action' - The type of stack set operation: @CREATE@, @UPDATE@, or @DELETE@. Create
-- and delete operations affect only the specified stack set instances that
-- are associated with the specified stack set. Update operations affect
-- both the stack set itself, as well as /all/ associated stack set
-- instances.
--
-- 'endTimestamp', 'stackSetOperation_endTimestamp' - The time at which the stack set operation ended, across all accounts and
-- Regions specified. Note that this doesn\'t necessarily mean that the
-- stack set operation was successful, or even attempted, in each account
-- or Region.
--
-- 'creationTimestamp', 'stackSetOperation_creationTimestamp' - The time at which the operation was initiated. Note that the creation
-- times for the stack set operation might differ from the creation time of
-- the individual stacks themselves. This is because CloudFormation needs
-- to perform preparatory work for the operation, such as dispatching the
-- work to the requested Regions, before actually creating the first
-- stacks.
--
-- 'operationPreferences', 'stackSetOperation_operationPreferences' - The preferences for how CloudFormation performs this stack set
-- operation.
--
-- 'operationId', 'stackSetOperation_operationId' - The unique ID of a stack set operation.
--
-- 'retainStacks', 'stackSetOperation_retainStacks' - For stack set operations of action type @DELETE@, specifies whether to
-- remove the stack instances from the specified stack set, but doesn\'t
-- delete the stacks. You can\'t reassociate a retained stack, or add an
-- existing, saved stack to a new stack set.
--
-- 'deploymentTargets', 'stackSetOperation_deploymentTargets' - [Service-managed permissions] The Organizations accounts affected by the
-- stack operation.
--
-- 'stackSetId', 'stackSetOperation_stackSetId' - The ID of the stack set.
--
-- 'executionRoleName', 'stackSetOperation_executionRoleName' - The name of the IAM execution role used to create or update the stack
-- set.
--
-- Use customized execution roles to control which stack resources users
-- and groups can include in their stack sets.
newStackSetOperation ::
  StackSetOperation
newStackSetOperation =
  StackSetOperation'
    { stackSetDriftDetectionDetails =
        Prelude.Nothing,
      status = Prelude.Nothing,
      administrationRoleARN = Prelude.Nothing,
      action = Prelude.Nothing,
      endTimestamp = Prelude.Nothing,
      creationTimestamp = Prelude.Nothing,
      operationPreferences = Prelude.Nothing,
      operationId = Prelude.Nothing,
      retainStacks = Prelude.Nothing,
      deploymentTargets = Prelude.Nothing,
      stackSetId = Prelude.Nothing,
      executionRoleName = Prelude.Nothing
    }

-- | Detailed information about the drift status of the stack set. This
-- includes information about drift operations currently being performed on
-- the stack set.
--
-- this information will only be present for stack set operations whose
-- @Action@ type is @DETECT_DRIFT@.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-drift.html Detecting Unmanaged Changes in Stack Sets>
-- in the CloudFormation User Guide.
stackSetOperation_stackSetDriftDetectionDetails :: Lens.Lens' StackSetOperation (Prelude.Maybe StackSetDriftDetectionDetails)
stackSetOperation_stackSetDriftDetectionDetails = Lens.lens (\StackSetOperation' {stackSetDriftDetectionDetails} -> stackSetDriftDetectionDetails) (\s@StackSetOperation' {} a -> s {stackSetDriftDetectionDetails = a} :: StackSetOperation)

-- | The status of the operation.
--
-- -   @FAILED@: The operation exceeded the specified failure tolerance.
--     The failure tolerance value that you\'ve set for an operation is
--     applied for each Region during stack create and update operations.
--     If the number of failed stacks within a Region exceeds the failure
--     tolerance, the status of the operation in the Region is set to
--     @FAILED@. This in turn sets the status of the operation as a whole
--     to @FAILED@, and CloudFormation cancels the operation in any
--     remaining Regions.
--
-- -   @QUEUED@: [Service-managed permissions] For automatic deployments
--     that require a sequence of operations, the operation is queued to be
--     performed. For more information, see the
--     <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-concepts.html#stackset-status-codes stack set operation status codes>
--     in the CloudFormation User Guide.
--
-- -   @RUNNING@: The operation is currently being performed.
--
-- -   @STOPPED@: The user has cancelled the operation.
--
-- -   @STOPPING@: The operation is in the process of stopping, at user
--     request.
--
-- -   @SUCCEEDED@: The operation completed creating or updating all the
--     specified stacks without exceeding the failure tolerance for the
--     operation.
stackSetOperation_status :: Lens.Lens' StackSetOperation (Prelude.Maybe StackSetOperationStatus)
stackSetOperation_status = Lens.lens (\StackSetOperation' {status} -> status) (\s@StackSetOperation' {} a -> s {status = a} :: StackSetOperation)

-- | The Amazon Resource Number (ARN) of the IAM role used to perform this
-- stack set operation.
--
-- Use customized administrator roles to control which users or groups can
-- manage specific stack sets within the same administrator account. For
-- more information, see
-- <http://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-prereqs.html Define Permissions for Multiple Administrators>
-- in the /CloudFormation User Guide/.
stackSetOperation_administrationRoleARN :: Lens.Lens' StackSetOperation (Prelude.Maybe Prelude.Text)
stackSetOperation_administrationRoleARN = Lens.lens (\StackSetOperation' {administrationRoleARN} -> administrationRoleARN) (\s@StackSetOperation' {} a -> s {administrationRoleARN = a} :: StackSetOperation)

-- | The type of stack set operation: @CREATE@, @UPDATE@, or @DELETE@. Create
-- and delete operations affect only the specified stack set instances that
-- are associated with the specified stack set. Update operations affect
-- both the stack set itself, as well as /all/ associated stack set
-- instances.
stackSetOperation_action :: Lens.Lens' StackSetOperation (Prelude.Maybe StackSetOperationAction)
stackSetOperation_action = Lens.lens (\StackSetOperation' {action} -> action) (\s@StackSetOperation' {} a -> s {action = a} :: StackSetOperation)

-- | The time at which the stack set operation ended, across all accounts and
-- Regions specified. Note that this doesn\'t necessarily mean that the
-- stack set operation was successful, or even attempted, in each account
-- or Region.
stackSetOperation_endTimestamp :: Lens.Lens' StackSetOperation (Prelude.Maybe Prelude.UTCTime)
stackSetOperation_endTimestamp = Lens.lens (\StackSetOperation' {endTimestamp} -> endTimestamp) (\s@StackSetOperation' {} a -> s {endTimestamp = a} :: StackSetOperation) Prelude.. Lens.mapping Core._Time

-- | The time at which the operation was initiated. Note that the creation
-- times for the stack set operation might differ from the creation time of
-- the individual stacks themselves. This is because CloudFormation needs
-- to perform preparatory work for the operation, such as dispatching the
-- work to the requested Regions, before actually creating the first
-- stacks.
stackSetOperation_creationTimestamp :: Lens.Lens' StackSetOperation (Prelude.Maybe Prelude.UTCTime)
stackSetOperation_creationTimestamp = Lens.lens (\StackSetOperation' {creationTimestamp} -> creationTimestamp) (\s@StackSetOperation' {} a -> s {creationTimestamp = a} :: StackSetOperation) Prelude.. Lens.mapping Core._Time

-- | The preferences for how CloudFormation performs this stack set
-- operation.
stackSetOperation_operationPreferences :: Lens.Lens' StackSetOperation (Prelude.Maybe StackSetOperationPreferences)
stackSetOperation_operationPreferences = Lens.lens (\StackSetOperation' {operationPreferences} -> operationPreferences) (\s@StackSetOperation' {} a -> s {operationPreferences = a} :: StackSetOperation)

-- | The unique ID of a stack set operation.
stackSetOperation_operationId :: Lens.Lens' StackSetOperation (Prelude.Maybe Prelude.Text)
stackSetOperation_operationId = Lens.lens (\StackSetOperation' {operationId} -> operationId) (\s@StackSetOperation' {} a -> s {operationId = a} :: StackSetOperation)

-- | For stack set operations of action type @DELETE@, specifies whether to
-- remove the stack instances from the specified stack set, but doesn\'t
-- delete the stacks. You can\'t reassociate a retained stack, or add an
-- existing, saved stack to a new stack set.
stackSetOperation_retainStacks :: Lens.Lens' StackSetOperation (Prelude.Maybe Prelude.Bool)
stackSetOperation_retainStacks = Lens.lens (\StackSetOperation' {retainStacks} -> retainStacks) (\s@StackSetOperation' {} a -> s {retainStacks = a} :: StackSetOperation)

-- | [Service-managed permissions] The Organizations accounts affected by the
-- stack operation.
stackSetOperation_deploymentTargets :: Lens.Lens' StackSetOperation (Prelude.Maybe DeploymentTargets)
stackSetOperation_deploymentTargets = Lens.lens (\StackSetOperation' {deploymentTargets} -> deploymentTargets) (\s@StackSetOperation' {} a -> s {deploymentTargets = a} :: StackSetOperation)

-- | The ID of the stack set.
stackSetOperation_stackSetId :: Lens.Lens' StackSetOperation (Prelude.Maybe Prelude.Text)
stackSetOperation_stackSetId = Lens.lens (\StackSetOperation' {stackSetId} -> stackSetId) (\s@StackSetOperation' {} a -> s {stackSetId = a} :: StackSetOperation)

-- | The name of the IAM execution role used to create or update the stack
-- set.
--
-- Use customized execution roles to control which stack resources users
-- and groups can include in their stack sets.
stackSetOperation_executionRoleName :: Lens.Lens' StackSetOperation (Prelude.Maybe Prelude.Text)
stackSetOperation_executionRoleName = Lens.lens (\StackSetOperation' {executionRoleName} -> executionRoleName) (\s@StackSetOperation' {} a -> s {executionRoleName = a} :: StackSetOperation)

instance Core.FromXML StackSetOperation where
  parseXML x =
    StackSetOperation'
      Prelude.<$> (x Core..@? "StackSetDriftDetectionDetails")
      Prelude.<*> (x Core..@? "Status")
      Prelude.<*> (x Core..@? "AdministrationRoleARN")
      Prelude.<*> (x Core..@? "Action")
      Prelude.<*> (x Core..@? "EndTimestamp")
      Prelude.<*> (x Core..@? "CreationTimestamp")
      Prelude.<*> (x Core..@? "OperationPreferences")
      Prelude.<*> (x Core..@? "OperationId")
      Prelude.<*> (x Core..@? "RetainStacks")
      Prelude.<*> (x Core..@? "DeploymentTargets")
      Prelude.<*> (x Core..@? "StackSetId")
      Prelude.<*> (x Core..@? "ExecutionRoleName")

instance Prelude.Hashable StackSetOperation where
  hashWithSalt salt' StackSetOperation' {..} =
    salt' `Prelude.hashWithSalt` executionRoleName
      `Prelude.hashWithSalt` stackSetId
      `Prelude.hashWithSalt` deploymentTargets
      `Prelude.hashWithSalt` retainStacks
      `Prelude.hashWithSalt` operationId
      `Prelude.hashWithSalt` operationPreferences
      `Prelude.hashWithSalt` creationTimestamp
      `Prelude.hashWithSalt` endTimestamp
      `Prelude.hashWithSalt` action
      `Prelude.hashWithSalt` administrationRoleARN
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` stackSetDriftDetectionDetails

instance Prelude.NFData StackSetOperation where
  rnf StackSetOperation' {..} =
    Prelude.rnf stackSetDriftDetectionDetails
      `Prelude.seq` Prelude.rnf executionRoleName
      `Prelude.seq` Prelude.rnf stackSetId
      `Prelude.seq` Prelude.rnf deploymentTargets
      `Prelude.seq` Prelude.rnf retainStacks
      `Prelude.seq` Prelude.rnf operationId
      `Prelude.seq` Prelude.rnf operationPreferences
      `Prelude.seq` Prelude.rnf creationTimestamp
      `Prelude.seq` Prelude.rnf endTimestamp
      `Prelude.seq` Prelude.rnf action
      `Prelude.seq` Prelude.rnf administrationRoleARN
      `Prelude.seq` Prelude.rnf status
