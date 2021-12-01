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
-- Module      : Amazonka.SSMIncidents.Types.RegionInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSMIncidents.Types.RegionInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SSMIncidents.Types.RegionStatus

-- | Information about a Region in your replication set.
--
-- /See:/ 'newRegionInfo' smart constructor.
data RegionInfo = RegionInfo'
  { -- | Information displayed about the status of the Region.
    statusMessage :: Prelude.Maybe Prelude.Text,
    -- | The ID of the KMS key used to encrypt the data in this Region.
    sseKmsKeyId :: Prelude.Maybe Prelude.Text,
    -- | The status of the Region in the replication set.
    status :: RegionStatus,
    -- | The most recent date and time that the Region\'s status was updated.
    statusUpdateDateTime :: Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RegionInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'statusMessage', 'regionInfo_statusMessage' - Information displayed about the status of the Region.
--
-- 'sseKmsKeyId', 'regionInfo_sseKmsKeyId' - The ID of the KMS key used to encrypt the data in this Region.
--
-- 'status', 'regionInfo_status' - The status of the Region in the replication set.
--
-- 'statusUpdateDateTime', 'regionInfo_statusUpdateDateTime' - The most recent date and time that the Region\'s status was updated.
newRegionInfo ::
  -- | 'status'
  RegionStatus ->
  -- | 'statusUpdateDateTime'
  Prelude.UTCTime ->
  RegionInfo
newRegionInfo pStatus_ pStatusUpdateDateTime_ =
  RegionInfo'
    { statusMessage = Prelude.Nothing,
      sseKmsKeyId = Prelude.Nothing,
      status = pStatus_,
      statusUpdateDateTime =
        Core._Time Lens.# pStatusUpdateDateTime_
    }

-- | Information displayed about the status of the Region.
regionInfo_statusMessage :: Lens.Lens' RegionInfo (Prelude.Maybe Prelude.Text)
regionInfo_statusMessage = Lens.lens (\RegionInfo' {statusMessage} -> statusMessage) (\s@RegionInfo' {} a -> s {statusMessage = a} :: RegionInfo)

-- | The ID of the KMS key used to encrypt the data in this Region.
regionInfo_sseKmsKeyId :: Lens.Lens' RegionInfo (Prelude.Maybe Prelude.Text)
regionInfo_sseKmsKeyId = Lens.lens (\RegionInfo' {sseKmsKeyId} -> sseKmsKeyId) (\s@RegionInfo' {} a -> s {sseKmsKeyId = a} :: RegionInfo)

-- | The status of the Region in the replication set.
regionInfo_status :: Lens.Lens' RegionInfo RegionStatus
regionInfo_status = Lens.lens (\RegionInfo' {status} -> status) (\s@RegionInfo' {} a -> s {status = a} :: RegionInfo)

-- | The most recent date and time that the Region\'s status was updated.
regionInfo_statusUpdateDateTime :: Lens.Lens' RegionInfo Prelude.UTCTime
regionInfo_statusUpdateDateTime = Lens.lens (\RegionInfo' {statusUpdateDateTime} -> statusUpdateDateTime) (\s@RegionInfo' {} a -> s {statusUpdateDateTime = a} :: RegionInfo) Prelude.. Core._Time

instance Core.FromJSON RegionInfo where
  parseJSON =
    Core.withObject
      "RegionInfo"
      ( \x ->
          RegionInfo'
            Prelude.<$> (x Core..:? "statusMessage")
            Prelude.<*> (x Core..:? "sseKmsKeyId")
            Prelude.<*> (x Core..: "status")
            Prelude.<*> (x Core..: "statusUpdateDateTime")
      )

instance Prelude.Hashable RegionInfo where
  hashWithSalt salt' RegionInfo' {..} =
    salt' `Prelude.hashWithSalt` statusUpdateDateTime
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` sseKmsKeyId
      `Prelude.hashWithSalt` statusMessage

instance Prelude.NFData RegionInfo where
  rnf RegionInfo' {..} =
    Prelude.rnf statusMessage
      `Prelude.seq` Prelude.rnf statusUpdateDateTime
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf sseKmsKeyId
