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
-- Module      : Amazonka.ECR.Types.ImageScanFindingsSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECR.Types.ImageScanFindingsSummary where

import qualified Amazonka.Core as Core
import Amazonka.ECR.Types.FindingSeverity
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | A summary of the last completed image scan.
--
-- /See:/ 'newImageScanFindingsSummary' smart constructor.
data ImageScanFindingsSummary = ImageScanFindingsSummary'
  { -- | The time of the last completed image scan.
    imageScanCompletedAt :: Prelude.Maybe Core.POSIX,
    -- | The image vulnerability counts, sorted by severity.
    findingSeverityCounts :: Prelude.Maybe (Prelude.HashMap FindingSeverity Prelude.Natural),
    -- | The time when the vulnerability data was last scanned.
    vulnerabilitySourceUpdatedAt :: Prelude.Maybe Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImageScanFindingsSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'imageScanCompletedAt', 'imageScanFindingsSummary_imageScanCompletedAt' - The time of the last completed image scan.
--
-- 'findingSeverityCounts', 'imageScanFindingsSummary_findingSeverityCounts' - The image vulnerability counts, sorted by severity.
--
-- 'vulnerabilitySourceUpdatedAt', 'imageScanFindingsSummary_vulnerabilitySourceUpdatedAt' - The time when the vulnerability data was last scanned.
newImageScanFindingsSummary ::
  ImageScanFindingsSummary
newImageScanFindingsSummary =
  ImageScanFindingsSummary'
    { imageScanCompletedAt =
        Prelude.Nothing,
      findingSeverityCounts = Prelude.Nothing,
      vulnerabilitySourceUpdatedAt = Prelude.Nothing
    }

-- | The time of the last completed image scan.
imageScanFindingsSummary_imageScanCompletedAt :: Lens.Lens' ImageScanFindingsSummary (Prelude.Maybe Prelude.UTCTime)
imageScanFindingsSummary_imageScanCompletedAt = Lens.lens (\ImageScanFindingsSummary' {imageScanCompletedAt} -> imageScanCompletedAt) (\s@ImageScanFindingsSummary' {} a -> s {imageScanCompletedAt = a} :: ImageScanFindingsSummary) Prelude.. Lens.mapping Core._Time

-- | The image vulnerability counts, sorted by severity.
imageScanFindingsSummary_findingSeverityCounts :: Lens.Lens' ImageScanFindingsSummary (Prelude.Maybe (Prelude.HashMap FindingSeverity Prelude.Natural))
imageScanFindingsSummary_findingSeverityCounts = Lens.lens (\ImageScanFindingsSummary' {findingSeverityCounts} -> findingSeverityCounts) (\s@ImageScanFindingsSummary' {} a -> s {findingSeverityCounts = a} :: ImageScanFindingsSummary) Prelude.. Lens.mapping Lens.coerced

-- | The time when the vulnerability data was last scanned.
imageScanFindingsSummary_vulnerabilitySourceUpdatedAt :: Lens.Lens' ImageScanFindingsSummary (Prelude.Maybe Prelude.UTCTime)
imageScanFindingsSummary_vulnerabilitySourceUpdatedAt = Lens.lens (\ImageScanFindingsSummary' {vulnerabilitySourceUpdatedAt} -> vulnerabilitySourceUpdatedAt) (\s@ImageScanFindingsSummary' {} a -> s {vulnerabilitySourceUpdatedAt = a} :: ImageScanFindingsSummary) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON ImageScanFindingsSummary where
  parseJSON =
    Core.withObject
      "ImageScanFindingsSummary"
      ( \x ->
          ImageScanFindingsSummary'
            Prelude.<$> (x Core..:? "imageScanCompletedAt")
            Prelude.<*> ( x Core..:? "findingSeverityCounts"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "vulnerabilitySourceUpdatedAt")
      )

instance Prelude.Hashable ImageScanFindingsSummary where
  hashWithSalt salt' ImageScanFindingsSummary' {..} =
    salt'
      `Prelude.hashWithSalt` vulnerabilitySourceUpdatedAt
      `Prelude.hashWithSalt` findingSeverityCounts
      `Prelude.hashWithSalt` imageScanCompletedAt

instance Prelude.NFData ImageScanFindingsSummary where
  rnf ImageScanFindingsSummary' {..} =
    Prelude.rnf imageScanCompletedAt
      `Prelude.seq` Prelude.rnf vulnerabilitySourceUpdatedAt
      `Prelude.seq` Prelude.rnf findingSeverityCounts
