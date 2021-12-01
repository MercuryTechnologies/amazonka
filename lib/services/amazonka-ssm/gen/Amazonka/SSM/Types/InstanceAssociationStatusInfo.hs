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
-- Module      : Amazonka.SSM.Types.InstanceAssociationStatusInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.InstanceAssociationStatusInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SSM.Types.InstanceAssociationOutputUrl

-- | Status information about the instance association.
--
-- /See:/ 'newInstanceAssociationStatusInfo' smart constructor.
data InstanceAssociationStatusInfo = InstanceAssociationStatusInfo'
  { -- | The association ID.
    associationId :: Prelude.Maybe Prelude.Text,
    -- | The instance ID where the association was created.
    instanceId :: Prelude.Maybe Prelude.Text,
    -- | Detailed status information about the instance association.
    detailedStatus :: Prelude.Maybe Prelude.Text,
    -- | Status information about the instance association.
    status :: Prelude.Maybe Prelude.Text,
    -- | A URL for an S3 bucket where you want to store the results of this
    -- request.
    outputUrl :: Prelude.Maybe InstanceAssociationOutputUrl,
    -- | Summary information about association execution.
    executionSummary :: Prelude.Maybe Prelude.Text,
    -- | The name of the association.
    name :: Prelude.Maybe Prelude.Text,
    -- | An error code returned by the request to create the association.
    errorCode :: Prelude.Maybe Prelude.Text,
    -- | The association document versions.
    documentVersion :: Prelude.Maybe Prelude.Text,
    -- | The version of the association applied to the instance.
    associationVersion :: Prelude.Maybe Prelude.Text,
    -- | The date the instance association ran.
    executionDate :: Prelude.Maybe Core.POSIX,
    -- | The name of the association applied to the instance.
    associationName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InstanceAssociationStatusInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'associationId', 'instanceAssociationStatusInfo_associationId' - The association ID.
--
-- 'instanceId', 'instanceAssociationStatusInfo_instanceId' - The instance ID where the association was created.
--
-- 'detailedStatus', 'instanceAssociationStatusInfo_detailedStatus' - Detailed status information about the instance association.
--
-- 'status', 'instanceAssociationStatusInfo_status' - Status information about the instance association.
--
-- 'outputUrl', 'instanceAssociationStatusInfo_outputUrl' - A URL for an S3 bucket where you want to store the results of this
-- request.
--
-- 'executionSummary', 'instanceAssociationStatusInfo_executionSummary' - Summary information about association execution.
--
-- 'name', 'instanceAssociationStatusInfo_name' - The name of the association.
--
-- 'errorCode', 'instanceAssociationStatusInfo_errorCode' - An error code returned by the request to create the association.
--
-- 'documentVersion', 'instanceAssociationStatusInfo_documentVersion' - The association document versions.
--
-- 'associationVersion', 'instanceAssociationStatusInfo_associationVersion' - The version of the association applied to the instance.
--
-- 'executionDate', 'instanceAssociationStatusInfo_executionDate' - The date the instance association ran.
--
-- 'associationName', 'instanceAssociationStatusInfo_associationName' - The name of the association applied to the instance.
newInstanceAssociationStatusInfo ::
  InstanceAssociationStatusInfo
newInstanceAssociationStatusInfo =
  InstanceAssociationStatusInfo'
    { associationId =
        Prelude.Nothing,
      instanceId = Prelude.Nothing,
      detailedStatus = Prelude.Nothing,
      status = Prelude.Nothing,
      outputUrl = Prelude.Nothing,
      executionSummary = Prelude.Nothing,
      name = Prelude.Nothing,
      errorCode = Prelude.Nothing,
      documentVersion = Prelude.Nothing,
      associationVersion = Prelude.Nothing,
      executionDate = Prelude.Nothing,
      associationName = Prelude.Nothing
    }

-- | The association ID.
instanceAssociationStatusInfo_associationId :: Lens.Lens' InstanceAssociationStatusInfo (Prelude.Maybe Prelude.Text)
instanceAssociationStatusInfo_associationId = Lens.lens (\InstanceAssociationStatusInfo' {associationId} -> associationId) (\s@InstanceAssociationStatusInfo' {} a -> s {associationId = a} :: InstanceAssociationStatusInfo)

-- | The instance ID where the association was created.
instanceAssociationStatusInfo_instanceId :: Lens.Lens' InstanceAssociationStatusInfo (Prelude.Maybe Prelude.Text)
instanceAssociationStatusInfo_instanceId = Lens.lens (\InstanceAssociationStatusInfo' {instanceId} -> instanceId) (\s@InstanceAssociationStatusInfo' {} a -> s {instanceId = a} :: InstanceAssociationStatusInfo)

-- | Detailed status information about the instance association.
instanceAssociationStatusInfo_detailedStatus :: Lens.Lens' InstanceAssociationStatusInfo (Prelude.Maybe Prelude.Text)
instanceAssociationStatusInfo_detailedStatus = Lens.lens (\InstanceAssociationStatusInfo' {detailedStatus} -> detailedStatus) (\s@InstanceAssociationStatusInfo' {} a -> s {detailedStatus = a} :: InstanceAssociationStatusInfo)

-- | Status information about the instance association.
instanceAssociationStatusInfo_status :: Lens.Lens' InstanceAssociationStatusInfo (Prelude.Maybe Prelude.Text)
instanceAssociationStatusInfo_status = Lens.lens (\InstanceAssociationStatusInfo' {status} -> status) (\s@InstanceAssociationStatusInfo' {} a -> s {status = a} :: InstanceAssociationStatusInfo)

-- | A URL for an S3 bucket where you want to store the results of this
-- request.
instanceAssociationStatusInfo_outputUrl :: Lens.Lens' InstanceAssociationStatusInfo (Prelude.Maybe InstanceAssociationOutputUrl)
instanceAssociationStatusInfo_outputUrl = Lens.lens (\InstanceAssociationStatusInfo' {outputUrl} -> outputUrl) (\s@InstanceAssociationStatusInfo' {} a -> s {outputUrl = a} :: InstanceAssociationStatusInfo)

-- | Summary information about association execution.
instanceAssociationStatusInfo_executionSummary :: Lens.Lens' InstanceAssociationStatusInfo (Prelude.Maybe Prelude.Text)
instanceAssociationStatusInfo_executionSummary = Lens.lens (\InstanceAssociationStatusInfo' {executionSummary} -> executionSummary) (\s@InstanceAssociationStatusInfo' {} a -> s {executionSummary = a} :: InstanceAssociationStatusInfo)

-- | The name of the association.
instanceAssociationStatusInfo_name :: Lens.Lens' InstanceAssociationStatusInfo (Prelude.Maybe Prelude.Text)
instanceAssociationStatusInfo_name = Lens.lens (\InstanceAssociationStatusInfo' {name} -> name) (\s@InstanceAssociationStatusInfo' {} a -> s {name = a} :: InstanceAssociationStatusInfo)

-- | An error code returned by the request to create the association.
instanceAssociationStatusInfo_errorCode :: Lens.Lens' InstanceAssociationStatusInfo (Prelude.Maybe Prelude.Text)
instanceAssociationStatusInfo_errorCode = Lens.lens (\InstanceAssociationStatusInfo' {errorCode} -> errorCode) (\s@InstanceAssociationStatusInfo' {} a -> s {errorCode = a} :: InstanceAssociationStatusInfo)

-- | The association document versions.
instanceAssociationStatusInfo_documentVersion :: Lens.Lens' InstanceAssociationStatusInfo (Prelude.Maybe Prelude.Text)
instanceAssociationStatusInfo_documentVersion = Lens.lens (\InstanceAssociationStatusInfo' {documentVersion} -> documentVersion) (\s@InstanceAssociationStatusInfo' {} a -> s {documentVersion = a} :: InstanceAssociationStatusInfo)

-- | The version of the association applied to the instance.
instanceAssociationStatusInfo_associationVersion :: Lens.Lens' InstanceAssociationStatusInfo (Prelude.Maybe Prelude.Text)
instanceAssociationStatusInfo_associationVersion = Lens.lens (\InstanceAssociationStatusInfo' {associationVersion} -> associationVersion) (\s@InstanceAssociationStatusInfo' {} a -> s {associationVersion = a} :: InstanceAssociationStatusInfo)

-- | The date the instance association ran.
instanceAssociationStatusInfo_executionDate :: Lens.Lens' InstanceAssociationStatusInfo (Prelude.Maybe Prelude.UTCTime)
instanceAssociationStatusInfo_executionDate = Lens.lens (\InstanceAssociationStatusInfo' {executionDate} -> executionDate) (\s@InstanceAssociationStatusInfo' {} a -> s {executionDate = a} :: InstanceAssociationStatusInfo) Prelude.. Lens.mapping Core._Time

-- | The name of the association applied to the instance.
instanceAssociationStatusInfo_associationName :: Lens.Lens' InstanceAssociationStatusInfo (Prelude.Maybe Prelude.Text)
instanceAssociationStatusInfo_associationName = Lens.lens (\InstanceAssociationStatusInfo' {associationName} -> associationName) (\s@InstanceAssociationStatusInfo' {} a -> s {associationName = a} :: InstanceAssociationStatusInfo)

instance Core.FromJSON InstanceAssociationStatusInfo where
  parseJSON =
    Core.withObject
      "InstanceAssociationStatusInfo"
      ( \x ->
          InstanceAssociationStatusInfo'
            Prelude.<$> (x Core..:? "AssociationId")
            Prelude.<*> (x Core..:? "InstanceId")
            Prelude.<*> (x Core..:? "DetailedStatus")
            Prelude.<*> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "OutputUrl")
            Prelude.<*> (x Core..:? "ExecutionSummary")
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "ErrorCode")
            Prelude.<*> (x Core..:? "DocumentVersion")
            Prelude.<*> (x Core..:? "AssociationVersion")
            Prelude.<*> (x Core..:? "ExecutionDate")
            Prelude.<*> (x Core..:? "AssociationName")
      )

instance
  Prelude.Hashable
    InstanceAssociationStatusInfo
  where
  hashWithSalt salt' InstanceAssociationStatusInfo' {..} =
    salt' `Prelude.hashWithSalt` associationName
      `Prelude.hashWithSalt` executionDate
      `Prelude.hashWithSalt` associationVersion
      `Prelude.hashWithSalt` documentVersion
      `Prelude.hashWithSalt` errorCode
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` executionSummary
      `Prelude.hashWithSalt` outputUrl
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` detailedStatus
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` associationId

instance Prelude.NFData InstanceAssociationStatusInfo where
  rnf InstanceAssociationStatusInfo' {..} =
    Prelude.rnf associationId
      `Prelude.seq` Prelude.rnf associationName
      `Prelude.seq` Prelude.rnf executionDate
      `Prelude.seq` Prelude.rnf associationVersion
      `Prelude.seq` Prelude.rnf documentVersion
      `Prelude.seq` Prelude.rnf errorCode
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf executionSummary
      `Prelude.seq` Prelude.rnf outputUrl
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf detailedStatus
      `Prelude.seq` Prelude.rnf instanceId
