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
-- Module      : Amazonka.SecurityHub.Types.Cvss
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.Cvss where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.Adjustment

-- | CVSS scores from the advisory related to the vulnerability.
--
-- /See:/ 'newCvss' smart constructor.
data Cvss = Cvss'
  { -- | Adjustments to the CVSS metrics.
    adjustments :: Prelude.Maybe [Adjustment],
    -- | The base scoring vector for the CVSS score.
    baseVector :: Prelude.Maybe Prelude.Text,
    -- | The version of CVSS for the CVSS score.
    version :: Prelude.Maybe Prelude.Text,
    -- | The origin of the original CVSS score and vector.
    source :: Prelude.Maybe Prelude.Text,
    -- | The base CVSS score.
    baseScore :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Cvss' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'adjustments', 'cvss_adjustments' - Adjustments to the CVSS metrics.
--
-- 'baseVector', 'cvss_baseVector' - The base scoring vector for the CVSS score.
--
-- 'version', 'cvss_version' - The version of CVSS for the CVSS score.
--
-- 'source', 'cvss_source' - The origin of the original CVSS score and vector.
--
-- 'baseScore', 'cvss_baseScore' - The base CVSS score.
newCvss ::
  Cvss
newCvss =
  Cvss'
    { adjustments = Prelude.Nothing,
      baseVector = Prelude.Nothing,
      version = Prelude.Nothing,
      source = Prelude.Nothing,
      baseScore = Prelude.Nothing
    }

-- | Adjustments to the CVSS metrics.
cvss_adjustments :: Lens.Lens' Cvss (Prelude.Maybe [Adjustment])
cvss_adjustments = Lens.lens (\Cvss' {adjustments} -> adjustments) (\s@Cvss' {} a -> s {adjustments = a} :: Cvss) Prelude.. Lens.mapping Lens.coerced

-- | The base scoring vector for the CVSS score.
cvss_baseVector :: Lens.Lens' Cvss (Prelude.Maybe Prelude.Text)
cvss_baseVector = Lens.lens (\Cvss' {baseVector} -> baseVector) (\s@Cvss' {} a -> s {baseVector = a} :: Cvss)

-- | The version of CVSS for the CVSS score.
cvss_version :: Lens.Lens' Cvss (Prelude.Maybe Prelude.Text)
cvss_version = Lens.lens (\Cvss' {version} -> version) (\s@Cvss' {} a -> s {version = a} :: Cvss)

-- | The origin of the original CVSS score and vector.
cvss_source :: Lens.Lens' Cvss (Prelude.Maybe Prelude.Text)
cvss_source = Lens.lens (\Cvss' {source} -> source) (\s@Cvss' {} a -> s {source = a} :: Cvss)

-- | The base CVSS score.
cvss_baseScore :: Lens.Lens' Cvss (Prelude.Maybe Prelude.Double)
cvss_baseScore = Lens.lens (\Cvss' {baseScore} -> baseScore) (\s@Cvss' {} a -> s {baseScore = a} :: Cvss)

instance Core.FromJSON Cvss where
  parseJSON =
    Core.withObject
      "Cvss"
      ( \x ->
          Cvss'
            Prelude.<$> (x Core..:? "Adjustments" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "BaseVector")
            Prelude.<*> (x Core..:? "Version")
            Prelude.<*> (x Core..:? "Source")
            Prelude.<*> (x Core..:? "BaseScore")
      )

instance Prelude.Hashable Cvss where
  hashWithSalt salt' Cvss' {..} =
    salt' `Prelude.hashWithSalt` baseScore
      `Prelude.hashWithSalt` source
      `Prelude.hashWithSalt` version
      `Prelude.hashWithSalt` baseVector
      `Prelude.hashWithSalt` adjustments

instance Prelude.NFData Cvss where
  rnf Cvss' {..} =
    Prelude.rnf adjustments
      `Prelude.seq` Prelude.rnf baseScore
      `Prelude.seq` Prelude.rnf source
      `Prelude.seq` Prelude.rnf version
      `Prelude.seq` Prelude.rnf baseVector

instance Core.ToJSON Cvss where
  toJSON Cvss' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Adjustments" Core..=) Prelude.<$> adjustments,
            ("BaseVector" Core..=) Prelude.<$> baseVector,
            ("Version" Core..=) Prelude.<$> version,
            ("Source" Core..=) Prelude.<$> source,
            ("BaseScore" Core..=) Prelude.<$> baseScore
          ]
      )
