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
-- Module      : Amazonka.IoT.Types.OTAUpdateSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoT.Types.OTAUpdateSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | An OTA update summary.
--
-- /See:/ 'newOTAUpdateSummary' smart constructor.
data OTAUpdateSummary = OTAUpdateSummary'
  { -- | The date when the OTA update was created.
    creationDate :: Prelude.Maybe Core.POSIX,
    -- | The OTA update ID.
    otaUpdateId :: Prelude.Maybe Prelude.Text,
    -- | The OTA update ARN.
    otaUpdateArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'OTAUpdateSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationDate', 'oTAUpdateSummary_creationDate' - The date when the OTA update was created.
--
-- 'otaUpdateId', 'oTAUpdateSummary_otaUpdateId' - The OTA update ID.
--
-- 'otaUpdateArn', 'oTAUpdateSummary_otaUpdateArn' - The OTA update ARN.
newOTAUpdateSummary ::
  OTAUpdateSummary
newOTAUpdateSummary =
  OTAUpdateSummary'
    { creationDate = Prelude.Nothing,
      otaUpdateId = Prelude.Nothing,
      otaUpdateArn = Prelude.Nothing
    }

-- | The date when the OTA update was created.
oTAUpdateSummary_creationDate :: Lens.Lens' OTAUpdateSummary (Prelude.Maybe Prelude.UTCTime)
oTAUpdateSummary_creationDate = Lens.lens (\OTAUpdateSummary' {creationDate} -> creationDate) (\s@OTAUpdateSummary' {} a -> s {creationDate = a} :: OTAUpdateSummary) Prelude.. Lens.mapping Core._Time

-- | The OTA update ID.
oTAUpdateSummary_otaUpdateId :: Lens.Lens' OTAUpdateSummary (Prelude.Maybe Prelude.Text)
oTAUpdateSummary_otaUpdateId = Lens.lens (\OTAUpdateSummary' {otaUpdateId} -> otaUpdateId) (\s@OTAUpdateSummary' {} a -> s {otaUpdateId = a} :: OTAUpdateSummary)

-- | The OTA update ARN.
oTAUpdateSummary_otaUpdateArn :: Lens.Lens' OTAUpdateSummary (Prelude.Maybe Prelude.Text)
oTAUpdateSummary_otaUpdateArn = Lens.lens (\OTAUpdateSummary' {otaUpdateArn} -> otaUpdateArn) (\s@OTAUpdateSummary' {} a -> s {otaUpdateArn = a} :: OTAUpdateSummary)

instance Core.FromJSON OTAUpdateSummary where
  parseJSON =
    Core.withObject
      "OTAUpdateSummary"
      ( \x ->
          OTAUpdateSummary'
            Prelude.<$> (x Core..:? "creationDate")
            Prelude.<*> (x Core..:? "otaUpdateId")
            Prelude.<*> (x Core..:? "otaUpdateArn")
      )

instance Prelude.Hashable OTAUpdateSummary where
  hashWithSalt salt' OTAUpdateSummary' {..} =
    salt' `Prelude.hashWithSalt` otaUpdateArn
      `Prelude.hashWithSalt` otaUpdateId
      `Prelude.hashWithSalt` creationDate

instance Prelude.NFData OTAUpdateSummary where
  rnf OTAUpdateSummary' {..} =
    Prelude.rnf creationDate
      `Prelude.seq` Prelude.rnf otaUpdateArn
      `Prelude.seq` Prelude.rnf otaUpdateId
