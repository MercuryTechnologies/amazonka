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
-- Module      : Amazonka.IoT.Types.OTAUpdateInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.OTAUpdateInfo where

import qualified Amazonka.Core as Core
import Amazonka.IoT.Types.AwsJobExecutionsRolloutConfig
import Amazonka.IoT.Types.AwsJobPresignedUrlConfig
import Amazonka.IoT.Types.ErrorInfo
import Amazonka.IoT.Types.OTAUpdateFile
import Amazonka.IoT.Types.OTAUpdateStatus
import Amazonka.IoT.Types.Protocol
import Amazonka.IoT.Types.TargetSelection
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information about an OTA update.
--
-- /See:/ 'newOTAUpdateInfo' smart constructor.
data OTAUpdateInfo = OTAUpdateInfo'
  { -- | The date when the OTA update was last updated.
    lastModifiedDate :: Prelude.Maybe Core.POSIX,
    -- | Configuration for the rollout of OTA updates.
    awsJobExecutionsRolloutConfig :: Prelude.Maybe AwsJobExecutionsRolloutConfig,
    -- | The IoT job ID associated with the OTA update.
    awsIotJobId :: Prelude.Maybe Prelude.Text,
    -- | The protocol used to transfer the OTA update image. Valid values are
    -- [HTTP], [MQTT], [HTTP, MQTT]. When both HTTP and MQTT are specified, the
    -- target device can choose the protocol.
    protocols :: Prelude.Maybe (Prelude.NonEmpty Protocol),
    -- | Configuration information for pre-signed URLs. Valid when @protocols@
    -- contains HTTP.
    awsJobPresignedUrlConfig :: Prelude.Maybe AwsJobPresignedUrlConfig,
    -- | A list of files associated with the OTA update.
    otaUpdateFiles :: Prelude.Maybe (Prelude.NonEmpty OTAUpdateFile),
    -- | The status of the OTA update.
    otaUpdateStatus :: Prelude.Maybe OTAUpdateStatus,
    -- | The targets of the OTA update.
    targets :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The IoT job ARN associated with the OTA update.
    awsIotJobArn :: Prelude.Maybe Prelude.Text,
    -- | The date when the OTA update was created.
    creationDate :: Prelude.Maybe Core.POSIX,
    -- | A collection of name\/value pairs
    additionalParameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The OTA update ID.
    otaUpdateId :: Prelude.Maybe Prelude.Text,
    -- | Error information associated with the OTA update.
    errorInfo :: Prelude.Maybe ErrorInfo,
    -- | The OTA update ARN.
    otaUpdateArn :: Prelude.Maybe Prelude.Text,
    -- | A description of the OTA update.
    description :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether the OTA update will continue to run (CONTINUOUS), or
    -- will be complete after all those things specified as targets have
    -- completed the OTA update (SNAPSHOT). If continuous, the OTA update may
    -- also be run on a thing when a change is detected in a target. For
    -- example, an OTA update will run on a thing when the thing is added to a
    -- target group, even after the OTA update was completed by all things
    -- originally in the group.
    targetSelection :: Prelude.Maybe TargetSelection
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OTAUpdateInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastModifiedDate', 'oTAUpdateInfo_lastModifiedDate' - The date when the OTA update was last updated.
--
-- 'awsJobExecutionsRolloutConfig', 'oTAUpdateInfo_awsJobExecutionsRolloutConfig' - Configuration for the rollout of OTA updates.
--
-- 'awsIotJobId', 'oTAUpdateInfo_awsIotJobId' - The IoT job ID associated with the OTA update.
--
-- 'protocols', 'oTAUpdateInfo_protocols' - The protocol used to transfer the OTA update image. Valid values are
-- [HTTP], [MQTT], [HTTP, MQTT]. When both HTTP and MQTT are specified, the
-- target device can choose the protocol.
--
-- 'awsJobPresignedUrlConfig', 'oTAUpdateInfo_awsJobPresignedUrlConfig' - Configuration information for pre-signed URLs. Valid when @protocols@
-- contains HTTP.
--
-- 'otaUpdateFiles', 'oTAUpdateInfo_otaUpdateFiles' - A list of files associated with the OTA update.
--
-- 'otaUpdateStatus', 'oTAUpdateInfo_otaUpdateStatus' - The status of the OTA update.
--
-- 'targets', 'oTAUpdateInfo_targets' - The targets of the OTA update.
--
-- 'awsIotJobArn', 'oTAUpdateInfo_awsIotJobArn' - The IoT job ARN associated with the OTA update.
--
-- 'creationDate', 'oTAUpdateInfo_creationDate' - The date when the OTA update was created.
--
-- 'additionalParameters', 'oTAUpdateInfo_additionalParameters' - A collection of name\/value pairs
--
-- 'otaUpdateId', 'oTAUpdateInfo_otaUpdateId' - The OTA update ID.
--
-- 'errorInfo', 'oTAUpdateInfo_errorInfo' - Error information associated with the OTA update.
--
-- 'otaUpdateArn', 'oTAUpdateInfo_otaUpdateArn' - The OTA update ARN.
--
-- 'description', 'oTAUpdateInfo_description' - A description of the OTA update.
--
-- 'targetSelection', 'oTAUpdateInfo_targetSelection' - Specifies whether the OTA update will continue to run (CONTINUOUS), or
-- will be complete after all those things specified as targets have
-- completed the OTA update (SNAPSHOT). If continuous, the OTA update may
-- also be run on a thing when a change is detected in a target. For
-- example, an OTA update will run on a thing when the thing is added to a
-- target group, even after the OTA update was completed by all things
-- originally in the group.
newOTAUpdateInfo ::
  OTAUpdateInfo
newOTAUpdateInfo =
  OTAUpdateInfo'
    { lastModifiedDate = Prelude.Nothing,
      awsJobExecutionsRolloutConfig = Prelude.Nothing,
      awsIotJobId = Prelude.Nothing,
      protocols = Prelude.Nothing,
      awsJobPresignedUrlConfig = Prelude.Nothing,
      otaUpdateFiles = Prelude.Nothing,
      otaUpdateStatus = Prelude.Nothing,
      targets = Prelude.Nothing,
      awsIotJobArn = Prelude.Nothing,
      creationDate = Prelude.Nothing,
      additionalParameters = Prelude.Nothing,
      otaUpdateId = Prelude.Nothing,
      errorInfo = Prelude.Nothing,
      otaUpdateArn = Prelude.Nothing,
      description = Prelude.Nothing,
      targetSelection = Prelude.Nothing
    }

-- | The date when the OTA update was last updated.
oTAUpdateInfo_lastModifiedDate :: Lens.Lens' OTAUpdateInfo (Prelude.Maybe Prelude.UTCTime)
oTAUpdateInfo_lastModifiedDate = Lens.lens (\OTAUpdateInfo' {lastModifiedDate} -> lastModifiedDate) (\s@OTAUpdateInfo' {} a -> s {lastModifiedDate = a} :: OTAUpdateInfo) Prelude.. Lens.mapping Core._Time

-- | Configuration for the rollout of OTA updates.
oTAUpdateInfo_awsJobExecutionsRolloutConfig :: Lens.Lens' OTAUpdateInfo (Prelude.Maybe AwsJobExecutionsRolloutConfig)
oTAUpdateInfo_awsJobExecutionsRolloutConfig = Lens.lens (\OTAUpdateInfo' {awsJobExecutionsRolloutConfig} -> awsJobExecutionsRolloutConfig) (\s@OTAUpdateInfo' {} a -> s {awsJobExecutionsRolloutConfig = a} :: OTAUpdateInfo)

-- | The IoT job ID associated with the OTA update.
oTAUpdateInfo_awsIotJobId :: Lens.Lens' OTAUpdateInfo (Prelude.Maybe Prelude.Text)
oTAUpdateInfo_awsIotJobId = Lens.lens (\OTAUpdateInfo' {awsIotJobId} -> awsIotJobId) (\s@OTAUpdateInfo' {} a -> s {awsIotJobId = a} :: OTAUpdateInfo)

-- | The protocol used to transfer the OTA update image. Valid values are
-- [HTTP], [MQTT], [HTTP, MQTT]. When both HTTP and MQTT are specified, the
-- target device can choose the protocol.
oTAUpdateInfo_protocols :: Lens.Lens' OTAUpdateInfo (Prelude.Maybe (Prelude.NonEmpty Protocol))
oTAUpdateInfo_protocols = Lens.lens (\OTAUpdateInfo' {protocols} -> protocols) (\s@OTAUpdateInfo' {} a -> s {protocols = a} :: OTAUpdateInfo) Prelude.. Lens.mapping Lens.coerced

-- | Configuration information for pre-signed URLs. Valid when @protocols@
-- contains HTTP.
oTAUpdateInfo_awsJobPresignedUrlConfig :: Lens.Lens' OTAUpdateInfo (Prelude.Maybe AwsJobPresignedUrlConfig)
oTAUpdateInfo_awsJobPresignedUrlConfig = Lens.lens (\OTAUpdateInfo' {awsJobPresignedUrlConfig} -> awsJobPresignedUrlConfig) (\s@OTAUpdateInfo' {} a -> s {awsJobPresignedUrlConfig = a} :: OTAUpdateInfo)

-- | A list of files associated with the OTA update.
oTAUpdateInfo_otaUpdateFiles :: Lens.Lens' OTAUpdateInfo (Prelude.Maybe (Prelude.NonEmpty OTAUpdateFile))
oTAUpdateInfo_otaUpdateFiles = Lens.lens (\OTAUpdateInfo' {otaUpdateFiles} -> otaUpdateFiles) (\s@OTAUpdateInfo' {} a -> s {otaUpdateFiles = a} :: OTAUpdateInfo) Prelude.. Lens.mapping Lens.coerced

-- | The status of the OTA update.
oTAUpdateInfo_otaUpdateStatus :: Lens.Lens' OTAUpdateInfo (Prelude.Maybe OTAUpdateStatus)
oTAUpdateInfo_otaUpdateStatus = Lens.lens (\OTAUpdateInfo' {otaUpdateStatus} -> otaUpdateStatus) (\s@OTAUpdateInfo' {} a -> s {otaUpdateStatus = a} :: OTAUpdateInfo)

-- | The targets of the OTA update.
oTAUpdateInfo_targets :: Lens.Lens' OTAUpdateInfo (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
oTAUpdateInfo_targets = Lens.lens (\OTAUpdateInfo' {targets} -> targets) (\s@OTAUpdateInfo' {} a -> s {targets = a} :: OTAUpdateInfo) Prelude.. Lens.mapping Lens.coerced

-- | The IoT job ARN associated with the OTA update.
oTAUpdateInfo_awsIotJobArn :: Lens.Lens' OTAUpdateInfo (Prelude.Maybe Prelude.Text)
oTAUpdateInfo_awsIotJobArn = Lens.lens (\OTAUpdateInfo' {awsIotJobArn} -> awsIotJobArn) (\s@OTAUpdateInfo' {} a -> s {awsIotJobArn = a} :: OTAUpdateInfo)

-- | The date when the OTA update was created.
oTAUpdateInfo_creationDate :: Lens.Lens' OTAUpdateInfo (Prelude.Maybe Prelude.UTCTime)
oTAUpdateInfo_creationDate = Lens.lens (\OTAUpdateInfo' {creationDate} -> creationDate) (\s@OTAUpdateInfo' {} a -> s {creationDate = a} :: OTAUpdateInfo) Prelude.. Lens.mapping Core._Time

-- | A collection of name\/value pairs
oTAUpdateInfo_additionalParameters :: Lens.Lens' OTAUpdateInfo (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
oTAUpdateInfo_additionalParameters = Lens.lens (\OTAUpdateInfo' {additionalParameters} -> additionalParameters) (\s@OTAUpdateInfo' {} a -> s {additionalParameters = a} :: OTAUpdateInfo) Prelude.. Lens.mapping Lens.coerced

-- | The OTA update ID.
oTAUpdateInfo_otaUpdateId :: Lens.Lens' OTAUpdateInfo (Prelude.Maybe Prelude.Text)
oTAUpdateInfo_otaUpdateId = Lens.lens (\OTAUpdateInfo' {otaUpdateId} -> otaUpdateId) (\s@OTAUpdateInfo' {} a -> s {otaUpdateId = a} :: OTAUpdateInfo)

-- | Error information associated with the OTA update.
oTAUpdateInfo_errorInfo :: Lens.Lens' OTAUpdateInfo (Prelude.Maybe ErrorInfo)
oTAUpdateInfo_errorInfo = Lens.lens (\OTAUpdateInfo' {errorInfo} -> errorInfo) (\s@OTAUpdateInfo' {} a -> s {errorInfo = a} :: OTAUpdateInfo)

-- | The OTA update ARN.
oTAUpdateInfo_otaUpdateArn :: Lens.Lens' OTAUpdateInfo (Prelude.Maybe Prelude.Text)
oTAUpdateInfo_otaUpdateArn = Lens.lens (\OTAUpdateInfo' {otaUpdateArn} -> otaUpdateArn) (\s@OTAUpdateInfo' {} a -> s {otaUpdateArn = a} :: OTAUpdateInfo)

-- | A description of the OTA update.
oTAUpdateInfo_description :: Lens.Lens' OTAUpdateInfo (Prelude.Maybe Prelude.Text)
oTAUpdateInfo_description = Lens.lens (\OTAUpdateInfo' {description} -> description) (\s@OTAUpdateInfo' {} a -> s {description = a} :: OTAUpdateInfo)

-- | Specifies whether the OTA update will continue to run (CONTINUOUS), or
-- will be complete after all those things specified as targets have
-- completed the OTA update (SNAPSHOT). If continuous, the OTA update may
-- also be run on a thing when a change is detected in a target. For
-- example, an OTA update will run on a thing when the thing is added to a
-- target group, even after the OTA update was completed by all things
-- originally in the group.
oTAUpdateInfo_targetSelection :: Lens.Lens' OTAUpdateInfo (Prelude.Maybe TargetSelection)
oTAUpdateInfo_targetSelection = Lens.lens (\OTAUpdateInfo' {targetSelection} -> targetSelection) (\s@OTAUpdateInfo' {} a -> s {targetSelection = a} :: OTAUpdateInfo)

instance Core.FromJSON OTAUpdateInfo where
  parseJSON =
    Core.withObject
      "OTAUpdateInfo"
      ( \x ->
          OTAUpdateInfo'
            Prelude.<$> (x Core..:? "lastModifiedDate")
            Prelude.<*> (x Core..:? "awsJobExecutionsRolloutConfig")
            Prelude.<*> (x Core..:? "awsIotJobId")
            Prelude.<*> (x Core..:? "protocols")
            Prelude.<*> (x Core..:? "awsJobPresignedUrlConfig")
            Prelude.<*> (x Core..:? "otaUpdateFiles")
            Prelude.<*> (x Core..:? "otaUpdateStatus")
            Prelude.<*> (x Core..:? "targets")
            Prelude.<*> (x Core..:? "awsIotJobArn")
            Prelude.<*> (x Core..:? "creationDate")
            Prelude.<*> ( x Core..:? "additionalParameters"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "otaUpdateId")
            Prelude.<*> (x Core..:? "errorInfo")
            Prelude.<*> (x Core..:? "otaUpdateArn")
            Prelude.<*> (x Core..:? "description")
            Prelude.<*> (x Core..:? "targetSelection")
      )

instance Prelude.Hashable OTAUpdateInfo where
  hashWithSalt salt' OTAUpdateInfo' {..} =
    salt' `Prelude.hashWithSalt` targetSelection
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` otaUpdateArn
      `Prelude.hashWithSalt` errorInfo
      `Prelude.hashWithSalt` otaUpdateId
      `Prelude.hashWithSalt` additionalParameters
      `Prelude.hashWithSalt` creationDate
      `Prelude.hashWithSalt` awsIotJobArn
      `Prelude.hashWithSalt` targets
      `Prelude.hashWithSalt` otaUpdateStatus
      `Prelude.hashWithSalt` otaUpdateFiles
      `Prelude.hashWithSalt` awsJobPresignedUrlConfig
      `Prelude.hashWithSalt` protocols
      `Prelude.hashWithSalt` awsIotJobId
      `Prelude.hashWithSalt` awsJobExecutionsRolloutConfig
      `Prelude.hashWithSalt` lastModifiedDate

instance Prelude.NFData OTAUpdateInfo where
  rnf OTAUpdateInfo' {..} =
    Prelude.rnf lastModifiedDate
      `Prelude.seq` Prelude.rnf targetSelection
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf otaUpdateArn
      `Prelude.seq` Prelude.rnf errorInfo
      `Prelude.seq` Prelude.rnf otaUpdateId
      `Prelude.seq` Prelude.rnf additionalParameters
      `Prelude.seq` Prelude.rnf creationDate
      `Prelude.seq` Prelude.rnf awsIotJobArn
      `Prelude.seq` Prelude.rnf targets
      `Prelude.seq` Prelude.rnf otaUpdateStatus
      `Prelude.seq` Prelude.rnf otaUpdateFiles
      `Prelude.seq` Prelude.rnf awsJobPresignedUrlConfig
      `Prelude.seq` Prelude.rnf protocols
      `Prelude.seq` Prelude.rnf awsIotJobId
      `Prelude.seq` Prelude.rnf awsJobExecutionsRolloutConfig
