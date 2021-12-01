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
-- Module      : Amazonka.CloudFormation.Types.StackSetDriftDetectionDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFormation.Types.StackSetDriftDetectionDetails where

import Amazonka.CloudFormation.Types.StackSetDriftDetectionStatus
import Amazonka.CloudFormation.Types.StackSetDriftStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Detailed information about the drift status of the stack set.
--
-- For stack sets, contains information about the last /completed/ drift
-- operation performed on the stack set. Information about drift operations
-- in-progress is not included.
--
-- For stack set operations, includes information about drift operations
-- currently being performed on the stack set.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/stacksets-drift.html Detecting Unmanaged Changes in Stack Sets>
-- in the /CloudFormation User Guide/.
--
-- /See:/ 'newStackSetDriftDetectionDetails' smart constructor.
data StackSetDriftDetectionDetails = StackSetDriftDetectionDetails'
  { -- | Most recent time when CloudFormation performed a drift detection
    -- operation on the stack set. This value will be @NULL@ for any stack set
    -- on which drift detection has not yet been performed.
    lastDriftCheckTimestamp :: Prelude.Maybe Core.ISO8601,
    -- | The total number of stack instances belonging to this stack set.
    --
    -- The total number of stack instances is equal to the total of:
    --
    -- -   Stack instances that match the stack set configuration.
    --
    -- -   Stack instances that have drifted from the stack set configuration.
    --
    -- -   Stack instances where the drift detection operation has failed.
    --
    -- -   Stack instances currently being checked for drift.
    totalStackInstancesCount :: Prelude.Maybe Prelude.Natural,
    -- | The number of stack instances that are currently being checked for
    -- drift.
    inProgressStackInstancesCount :: Prelude.Maybe Prelude.Natural,
    -- | The number of stack instances that have drifted from the expected
    -- template and parameter configuration of the stack set. A stack instance
    -- is considered to have drifted if one or more of the resources in the
    -- associated stack do not match their expected configuration.
    driftedStackInstancesCount :: Prelude.Maybe Prelude.Natural,
    -- | The status of the stack set drift detection operation.
    --
    -- -   @COMPLETED@: The drift detection operation completed without failing
    --     on any stack instances.
    --
    -- -   @FAILED@: The drift detection operation exceeded the specified
    --     failure tolerance.
    --
    -- -   @PARTIAL_SUCCESS@: The drift detection operation completed without
    --     exceeding the failure tolerance for the operation.
    --
    -- -   @IN_PROGRESS@: The drift detection operation is currently being
    --     performed.
    --
    -- -   @STOPPED@: The user has cancelled the drift detection operation.
    driftDetectionStatus :: Prelude.Maybe StackSetDriftDetectionStatus,
    -- | Status of the stack set\'s actual configuration compared to its expected
    -- template and parameter configuration. A stack set is considered to have
    -- drifted if one or more of its stack instances have drifted from their
    -- expected template and parameter configuration.
    --
    -- -   @DRIFTED@: One or more of the stack instances belonging to the stack
    --     set stack differs from the expected template and parameter
    --     configuration. A stack instance is considered to have drifted if one
    --     or more of the resources in the associated stack have drifted.
    --
    -- -   @NOT_CHECKED@: CloudFormation has not checked the stack set for
    --     drift.
    --
    -- -   @IN_SYNC@: All of the stack instances belonging to the stack set
    --     stack match from the expected template and parameter configuration.
    driftStatus :: Prelude.Maybe StackSetDriftStatus,
    -- | The number of stack instances for which the drift detection operation
    -- failed.
    failedStackInstancesCount :: Prelude.Maybe Prelude.Natural,
    -- | The number of stack instances which match the expected template and
    -- parameter configuration of the stack set.
    inSyncStackInstancesCount :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StackSetDriftDetectionDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastDriftCheckTimestamp', 'stackSetDriftDetectionDetails_lastDriftCheckTimestamp' - Most recent time when CloudFormation performed a drift detection
-- operation on the stack set. This value will be @NULL@ for any stack set
-- on which drift detection has not yet been performed.
--
-- 'totalStackInstancesCount', 'stackSetDriftDetectionDetails_totalStackInstancesCount' - The total number of stack instances belonging to this stack set.
--
-- The total number of stack instances is equal to the total of:
--
-- -   Stack instances that match the stack set configuration.
--
-- -   Stack instances that have drifted from the stack set configuration.
--
-- -   Stack instances where the drift detection operation has failed.
--
-- -   Stack instances currently being checked for drift.
--
-- 'inProgressStackInstancesCount', 'stackSetDriftDetectionDetails_inProgressStackInstancesCount' - The number of stack instances that are currently being checked for
-- drift.
--
-- 'driftedStackInstancesCount', 'stackSetDriftDetectionDetails_driftedStackInstancesCount' - The number of stack instances that have drifted from the expected
-- template and parameter configuration of the stack set. A stack instance
-- is considered to have drifted if one or more of the resources in the
-- associated stack do not match their expected configuration.
--
-- 'driftDetectionStatus', 'stackSetDriftDetectionDetails_driftDetectionStatus' - The status of the stack set drift detection operation.
--
-- -   @COMPLETED@: The drift detection operation completed without failing
--     on any stack instances.
--
-- -   @FAILED@: The drift detection operation exceeded the specified
--     failure tolerance.
--
-- -   @PARTIAL_SUCCESS@: The drift detection operation completed without
--     exceeding the failure tolerance for the operation.
--
-- -   @IN_PROGRESS@: The drift detection operation is currently being
--     performed.
--
-- -   @STOPPED@: The user has cancelled the drift detection operation.
--
-- 'driftStatus', 'stackSetDriftDetectionDetails_driftStatus' - Status of the stack set\'s actual configuration compared to its expected
-- template and parameter configuration. A stack set is considered to have
-- drifted if one or more of its stack instances have drifted from their
-- expected template and parameter configuration.
--
-- -   @DRIFTED@: One or more of the stack instances belonging to the stack
--     set stack differs from the expected template and parameter
--     configuration. A stack instance is considered to have drifted if one
--     or more of the resources in the associated stack have drifted.
--
-- -   @NOT_CHECKED@: CloudFormation has not checked the stack set for
--     drift.
--
-- -   @IN_SYNC@: All of the stack instances belonging to the stack set
--     stack match from the expected template and parameter configuration.
--
-- 'failedStackInstancesCount', 'stackSetDriftDetectionDetails_failedStackInstancesCount' - The number of stack instances for which the drift detection operation
-- failed.
--
-- 'inSyncStackInstancesCount', 'stackSetDriftDetectionDetails_inSyncStackInstancesCount' - The number of stack instances which match the expected template and
-- parameter configuration of the stack set.
newStackSetDriftDetectionDetails ::
  StackSetDriftDetectionDetails
newStackSetDriftDetectionDetails =
  StackSetDriftDetectionDetails'
    { lastDriftCheckTimestamp =
        Prelude.Nothing,
      totalStackInstancesCount = Prelude.Nothing,
      inProgressStackInstancesCount =
        Prelude.Nothing,
      driftedStackInstancesCount = Prelude.Nothing,
      driftDetectionStatus = Prelude.Nothing,
      driftStatus = Prelude.Nothing,
      failedStackInstancesCount = Prelude.Nothing,
      inSyncStackInstancesCount = Prelude.Nothing
    }

-- | Most recent time when CloudFormation performed a drift detection
-- operation on the stack set. This value will be @NULL@ for any stack set
-- on which drift detection has not yet been performed.
stackSetDriftDetectionDetails_lastDriftCheckTimestamp :: Lens.Lens' StackSetDriftDetectionDetails (Prelude.Maybe Prelude.UTCTime)
stackSetDriftDetectionDetails_lastDriftCheckTimestamp = Lens.lens (\StackSetDriftDetectionDetails' {lastDriftCheckTimestamp} -> lastDriftCheckTimestamp) (\s@StackSetDriftDetectionDetails' {} a -> s {lastDriftCheckTimestamp = a} :: StackSetDriftDetectionDetails) Prelude.. Lens.mapping Core._Time

-- | The total number of stack instances belonging to this stack set.
--
-- The total number of stack instances is equal to the total of:
--
-- -   Stack instances that match the stack set configuration.
--
-- -   Stack instances that have drifted from the stack set configuration.
--
-- -   Stack instances where the drift detection operation has failed.
--
-- -   Stack instances currently being checked for drift.
stackSetDriftDetectionDetails_totalStackInstancesCount :: Lens.Lens' StackSetDriftDetectionDetails (Prelude.Maybe Prelude.Natural)
stackSetDriftDetectionDetails_totalStackInstancesCount = Lens.lens (\StackSetDriftDetectionDetails' {totalStackInstancesCount} -> totalStackInstancesCount) (\s@StackSetDriftDetectionDetails' {} a -> s {totalStackInstancesCount = a} :: StackSetDriftDetectionDetails)

-- | The number of stack instances that are currently being checked for
-- drift.
stackSetDriftDetectionDetails_inProgressStackInstancesCount :: Lens.Lens' StackSetDriftDetectionDetails (Prelude.Maybe Prelude.Natural)
stackSetDriftDetectionDetails_inProgressStackInstancesCount = Lens.lens (\StackSetDriftDetectionDetails' {inProgressStackInstancesCount} -> inProgressStackInstancesCount) (\s@StackSetDriftDetectionDetails' {} a -> s {inProgressStackInstancesCount = a} :: StackSetDriftDetectionDetails)

-- | The number of stack instances that have drifted from the expected
-- template and parameter configuration of the stack set. A stack instance
-- is considered to have drifted if one or more of the resources in the
-- associated stack do not match their expected configuration.
stackSetDriftDetectionDetails_driftedStackInstancesCount :: Lens.Lens' StackSetDriftDetectionDetails (Prelude.Maybe Prelude.Natural)
stackSetDriftDetectionDetails_driftedStackInstancesCount = Lens.lens (\StackSetDriftDetectionDetails' {driftedStackInstancesCount} -> driftedStackInstancesCount) (\s@StackSetDriftDetectionDetails' {} a -> s {driftedStackInstancesCount = a} :: StackSetDriftDetectionDetails)

-- | The status of the stack set drift detection operation.
--
-- -   @COMPLETED@: The drift detection operation completed without failing
--     on any stack instances.
--
-- -   @FAILED@: The drift detection operation exceeded the specified
--     failure tolerance.
--
-- -   @PARTIAL_SUCCESS@: The drift detection operation completed without
--     exceeding the failure tolerance for the operation.
--
-- -   @IN_PROGRESS@: The drift detection operation is currently being
--     performed.
--
-- -   @STOPPED@: The user has cancelled the drift detection operation.
stackSetDriftDetectionDetails_driftDetectionStatus :: Lens.Lens' StackSetDriftDetectionDetails (Prelude.Maybe StackSetDriftDetectionStatus)
stackSetDriftDetectionDetails_driftDetectionStatus = Lens.lens (\StackSetDriftDetectionDetails' {driftDetectionStatus} -> driftDetectionStatus) (\s@StackSetDriftDetectionDetails' {} a -> s {driftDetectionStatus = a} :: StackSetDriftDetectionDetails)

-- | Status of the stack set\'s actual configuration compared to its expected
-- template and parameter configuration. A stack set is considered to have
-- drifted if one or more of its stack instances have drifted from their
-- expected template and parameter configuration.
--
-- -   @DRIFTED@: One or more of the stack instances belonging to the stack
--     set stack differs from the expected template and parameter
--     configuration. A stack instance is considered to have drifted if one
--     or more of the resources in the associated stack have drifted.
--
-- -   @NOT_CHECKED@: CloudFormation has not checked the stack set for
--     drift.
--
-- -   @IN_SYNC@: All of the stack instances belonging to the stack set
--     stack match from the expected template and parameter configuration.
stackSetDriftDetectionDetails_driftStatus :: Lens.Lens' StackSetDriftDetectionDetails (Prelude.Maybe StackSetDriftStatus)
stackSetDriftDetectionDetails_driftStatus = Lens.lens (\StackSetDriftDetectionDetails' {driftStatus} -> driftStatus) (\s@StackSetDriftDetectionDetails' {} a -> s {driftStatus = a} :: StackSetDriftDetectionDetails)

-- | The number of stack instances for which the drift detection operation
-- failed.
stackSetDriftDetectionDetails_failedStackInstancesCount :: Lens.Lens' StackSetDriftDetectionDetails (Prelude.Maybe Prelude.Natural)
stackSetDriftDetectionDetails_failedStackInstancesCount = Lens.lens (\StackSetDriftDetectionDetails' {failedStackInstancesCount} -> failedStackInstancesCount) (\s@StackSetDriftDetectionDetails' {} a -> s {failedStackInstancesCount = a} :: StackSetDriftDetectionDetails)

-- | The number of stack instances which match the expected template and
-- parameter configuration of the stack set.
stackSetDriftDetectionDetails_inSyncStackInstancesCount :: Lens.Lens' StackSetDriftDetectionDetails (Prelude.Maybe Prelude.Natural)
stackSetDriftDetectionDetails_inSyncStackInstancesCount = Lens.lens (\StackSetDriftDetectionDetails' {inSyncStackInstancesCount} -> inSyncStackInstancesCount) (\s@StackSetDriftDetectionDetails' {} a -> s {inSyncStackInstancesCount = a} :: StackSetDriftDetectionDetails)

instance Core.FromXML StackSetDriftDetectionDetails where
  parseXML x =
    StackSetDriftDetectionDetails'
      Prelude.<$> (x Core..@? "LastDriftCheckTimestamp")
      Prelude.<*> (x Core..@? "TotalStackInstancesCount")
      Prelude.<*> (x Core..@? "InProgressStackInstancesCount")
      Prelude.<*> (x Core..@? "DriftedStackInstancesCount")
      Prelude.<*> (x Core..@? "DriftDetectionStatus")
      Prelude.<*> (x Core..@? "DriftStatus")
      Prelude.<*> (x Core..@? "FailedStackInstancesCount")
      Prelude.<*> (x Core..@? "InSyncStackInstancesCount")

instance
  Prelude.Hashable
    StackSetDriftDetectionDetails
  where
  hashWithSalt salt' StackSetDriftDetectionDetails' {..} =
    salt'
      `Prelude.hashWithSalt` inSyncStackInstancesCount
      `Prelude.hashWithSalt` failedStackInstancesCount
      `Prelude.hashWithSalt` driftStatus
      `Prelude.hashWithSalt` driftDetectionStatus
      `Prelude.hashWithSalt` driftedStackInstancesCount
      `Prelude.hashWithSalt` inProgressStackInstancesCount
      `Prelude.hashWithSalt` totalStackInstancesCount
      `Prelude.hashWithSalt` lastDriftCheckTimestamp

instance Prelude.NFData StackSetDriftDetectionDetails where
  rnf StackSetDriftDetectionDetails' {..} =
    Prelude.rnf lastDriftCheckTimestamp
      `Prelude.seq` Prelude.rnf inSyncStackInstancesCount
      `Prelude.seq` Prelude.rnf failedStackInstancesCount
      `Prelude.seq` Prelude.rnf driftStatus
      `Prelude.seq` Prelude.rnf driftDetectionStatus
      `Prelude.seq` Prelude.rnf driftedStackInstancesCount
      `Prelude.seq` Prelude.rnf inProgressStackInstancesCount
      `Prelude.seq` Prelude.rnf totalStackInstancesCount
