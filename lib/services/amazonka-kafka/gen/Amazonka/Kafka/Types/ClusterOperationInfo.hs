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
-- Module      : Amazonka.Kafka.Types.ClusterOperationInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kafka.Types.ClusterOperationInfo where

import qualified Amazonka.Core as Core
import Amazonka.Kafka.Types.ClusterOperationStep
import Amazonka.Kafka.Types.ErrorInfo
import Amazonka.Kafka.Types.MutableClusterInfo
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Returns information about a cluster operation.
--
-- /See:/ 'newClusterOperationInfo' smart constructor.
data ClusterOperationInfo = ClusterOperationInfo'
  { -- | The time that the operation was created.
    creationTime :: Prelude.Maybe Core.POSIX,
    -- | ARN of the cluster.
    clusterArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the API request that triggered this operation.
    clientRequestId :: Prelude.Maybe Prelude.Text,
    -- | Information about cluster attributes after a cluster is updated.
    targetClusterInfo :: Prelude.Maybe MutableClusterInfo,
    -- | Information about cluster attributes before a cluster is updated.
    sourceClusterInfo :: Prelude.Maybe MutableClusterInfo,
    -- | Steps completed during the operation.
    operationSteps :: Prelude.Maybe [ClusterOperationStep],
    -- | State of the cluster operation.
    operationState :: Prelude.Maybe Prelude.Text,
    -- | The time at which the operation finished.
    endTime :: Prelude.Maybe Core.POSIX,
    -- | Type of the cluster operation.
    operationType :: Prelude.Maybe Prelude.Text,
    -- | ARN of the cluster operation.
    operationArn :: Prelude.Maybe Prelude.Text,
    -- | Describes the error if the operation fails.
    errorInfo :: Prelude.Maybe ErrorInfo
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ClusterOperationInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'clusterOperationInfo_creationTime' - The time that the operation was created.
--
-- 'clusterArn', 'clusterOperationInfo_clusterArn' - ARN of the cluster.
--
-- 'clientRequestId', 'clusterOperationInfo_clientRequestId' - The ID of the API request that triggered this operation.
--
-- 'targetClusterInfo', 'clusterOperationInfo_targetClusterInfo' - Information about cluster attributes after a cluster is updated.
--
-- 'sourceClusterInfo', 'clusterOperationInfo_sourceClusterInfo' - Information about cluster attributes before a cluster is updated.
--
-- 'operationSteps', 'clusterOperationInfo_operationSteps' - Steps completed during the operation.
--
-- 'operationState', 'clusterOperationInfo_operationState' - State of the cluster operation.
--
-- 'endTime', 'clusterOperationInfo_endTime' - The time at which the operation finished.
--
-- 'operationType', 'clusterOperationInfo_operationType' - Type of the cluster operation.
--
-- 'operationArn', 'clusterOperationInfo_operationArn' - ARN of the cluster operation.
--
-- 'errorInfo', 'clusterOperationInfo_errorInfo' - Describes the error if the operation fails.
newClusterOperationInfo ::
  ClusterOperationInfo
newClusterOperationInfo =
  ClusterOperationInfo'
    { creationTime =
        Prelude.Nothing,
      clusterArn = Prelude.Nothing,
      clientRequestId = Prelude.Nothing,
      targetClusterInfo = Prelude.Nothing,
      sourceClusterInfo = Prelude.Nothing,
      operationSteps = Prelude.Nothing,
      operationState = Prelude.Nothing,
      endTime = Prelude.Nothing,
      operationType = Prelude.Nothing,
      operationArn = Prelude.Nothing,
      errorInfo = Prelude.Nothing
    }

-- | The time that the operation was created.
clusterOperationInfo_creationTime :: Lens.Lens' ClusterOperationInfo (Prelude.Maybe Prelude.UTCTime)
clusterOperationInfo_creationTime = Lens.lens (\ClusterOperationInfo' {creationTime} -> creationTime) (\s@ClusterOperationInfo' {} a -> s {creationTime = a} :: ClusterOperationInfo) Prelude.. Lens.mapping Core._Time

-- | ARN of the cluster.
clusterOperationInfo_clusterArn :: Lens.Lens' ClusterOperationInfo (Prelude.Maybe Prelude.Text)
clusterOperationInfo_clusterArn = Lens.lens (\ClusterOperationInfo' {clusterArn} -> clusterArn) (\s@ClusterOperationInfo' {} a -> s {clusterArn = a} :: ClusterOperationInfo)

-- | The ID of the API request that triggered this operation.
clusterOperationInfo_clientRequestId :: Lens.Lens' ClusterOperationInfo (Prelude.Maybe Prelude.Text)
clusterOperationInfo_clientRequestId = Lens.lens (\ClusterOperationInfo' {clientRequestId} -> clientRequestId) (\s@ClusterOperationInfo' {} a -> s {clientRequestId = a} :: ClusterOperationInfo)

-- | Information about cluster attributes after a cluster is updated.
clusterOperationInfo_targetClusterInfo :: Lens.Lens' ClusterOperationInfo (Prelude.Maybe MutableClusterInfo)
clusterOperationInfo_targetClusterInfo = Lens.lens (\ClusterOperationInfo' {targetClusterInfo} -> targetClusterInfo) (\s@ClusterOperationInfo' {} a -> s {targetClusterInfo = a} :: ClusterOperationInfo)

-- | Information about cluster attributes before a cluster is updated.
clusterOperationInfo_sourceClusterInfo :: Lens.Lens' ClusterOperationInfo (Prelude.Maybe MutableClusterInfo)
clusterOperationInfo_sourceClusterInfo = Lens.lens (\ClusterOperationInfo' {sourceClusterInfo} -> sourceClusterInfo) (\s@ClusterOperationInfo' {} a -> s {sourceClusterInfo = a} :: ClusterOperationInfo)

-- | Steps completed during the operation.
clusterOperationInfo_operationSteps :: Lens.Lens' ClusterOperationInfo (Prelude.Maybe [ClusterOperationStep])
clusterOperationInfo_operationSteps = Lens.lens (\ClusterOperationInfo' {operationSteps} -> operationSteps) (\s@ClusterOperationInfo' {} a -> s {operationSteps = a} :: ClusterOperationInfo) Prelude.. Lens.mapping Lens.coerced

-- | State of the cluster operation.
clusterOperationInfo_operationState :: Lens.Lens' ClusterOperationInfo (Prelude.Maybe Prelude.Text)
clusterOperationInfo_operationState = Lens.lens (\ClusterOperationInfo' {operationState} -> operationState) (\s@ClusterOperationInfo' {} a -> s {operationState = a} :: ClusterOperationInfo)

-- | The time at which the operation finished.
clusterOperationInfo_endTime :: Lens.Lens' ClusterOperationInfo (Prelude.Maybe Prelude.UTCTime)
clusterOperationInfo_endTime = Lens.lens (\ClusterOperationInfo' {endTime} -> endTime) (\s@ClusterOperationInfo' {} a -> s {endTime = a} :: ClusterOperationInfo) Prelude.. Lens.mapping Core._Time

-- | Type of the cluster operation.
clusterOperationInfo_operationType :: Lens.Lens' ClusterOperationInfo (Prelude.Maybe Prelude.Text)
clusterOperationInfo_operationType = Lens.lens (\ClusterOperationInfo' {operationType} -> operationType) (\s@ClusterOperationInfo' {} a -> s {operationType = a} :: ClusterOperationInfo)

-- | ARN of the cluster operation.
clusterOperationInfo_operationArn :: Lens.Lens' ClusterOperationInfo (Prelude.Maybe Prelude.Text)
clusterOperationInfo_operationArn = Lens.lens (\ClusterOperationInfo' {operationArn} -> operationArn) (\s@ClusterOperationInfo' {} a -> s {operationArn = a} :: ClusterOperationInfo)

-- | Describes the error if the operation fails.
clusterOperationInfo_errorInfo :: Lens.Lens' ClusterOperationInfo (Prelude.Maybe ErrorInfo)
clusterOperationInfo_errorInfo = Lens.lens (\ClusterOperationInfo' {errorInfo} -> errorInfo) (\s@ClusterOperationInfo' {} a -> s {errorInfo = a} :: ClusterOperationInfo)

instance Core.FromJSON ClusterOperationInfo where
  parseJSON =
    Core.withObject
      "ClusterOperationInfo"
      ( \x ->
          ClusterOperationInfo'
            Prelude.<$> (x Core..:? "creationTime")
            Prelude.<*> (x Core..:? "clusterArn")
            Prelude.<*> (x Core..:? "clientRequestId")
            Prelude.<*> (x Core..:? "targetClusterInfo")
            Prelude.<*> (x Core..:? "sourceClusterInfo")
            Prelude.<*> (x Core..:? "operationSteps" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "operationState")
            Prelude.<*> (x Core..:? "endTime")
            Prelude.<*> (x Core..:? "operationType")
            Prelude.<*> (x Core..:? "operationArn")
            Prelude.<*> (x Core..:? "errorInfo")
      )

instance Prelude.Hashable ClusterOperationInfo where
  hashWithSalt salt' ClusterOperationInfo' {..} =
    salt' `Prelude.hashWithSalt` errorInfo
      `Prelude.hashWithSalt` operationArn
      `Prelude.hashWithSalt` operationType
      `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` operationState
      `Prelude.hashWithSalt` operationSteps
      `Prelude.hashWithSalt` sourceClusterInfo
      `Prelude.hashWithSalt` targetClusterInfo
      `Prelude.hashWithSalt` clientRequestId
      `Prelude.hashWithSalt` clusterArn
      `Prelude.hashWithSalt` creationTime

instance Prelude.NFData ClusterOperationInfo where
  rnf ClusterOperationInfo' {..} =
    Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf errorInfo
      `Prelude.seq` Prelude.rnf operationArn
      `Prelude.seq` Prelude.rnf operationType
      `Prelude.seq` Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf operationState
      `Prelude.seq` Prelude.rnf operationSteps
      `Prelude.seq` Prelude.rnf sourceClusterInfo
      `Prelude.seq` Prelude.rnf targetClusterInfo
      `Prelude.seq` Prelude.rnf clientRequestId
      `Prelude.seq` Prelude.rnf clusterArn
