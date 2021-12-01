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
-- Module      : Amazonka.MediaPackage.Types.MssPackage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaPackage.Types.MssPackage where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.MediaPackage.Types.MssEncryption
import Amazonka.MediaPackage.Types.StreamSelection
import qualified Amazonka.Prelude as Prelude

-- | A Microsoft Smooth Streaming (MSS) packaging configuration.
--
-- /See:/ 'newMssPackage' smart constructor.
data MssPackage = MssPackage'
  { -- | The duration (in seconds) of each segment.
    segmentDurationSeconds :: Prelude.Maybe Prelude.Int,
    streamSelection :: Prelude.Maybe StreamSelection,
    encryption :: Prelude.Maybe MssEncryption,
    -- | The time window (in seconds) contained in each manifest.
    manifestWindowSeconds :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MssPackage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'segmentDurationSeconds', 'mssPackage_segmentDurationSeconds' - The duration (in seconds) of each segment.
--
-- 'streamSelection', 'mssPackage_streamSelection' - Undocumented member.
--
-- 'encryption', 'mssPackage_encryption' - Undocumented member.
--
-- 'manifestWindowSeconds', 'mssPackage_manifestWindowSeconds' - The time window (in seconds) contained in each manifest.
newMssPackage ::
  MssPackage
newMssPackage =
  MssPackage'
    { segmentDurationSeconds =
        Prelude.Nothing,
      streamSelection = Prelude.Nothing,
      encryption = Prelude.Nothing,
      manifestWindowSeconds = Prelude.Nothing
    }

-- | The duration (in seconds) of each segment.
mssPackage_segmentDurationSeconds :: Lens.Lens' MssPackage (Prelude.Maybe Prelude.Int)
mssPackage_segmentDurationSeconds = Lens.lens (\MssPackage' {segmentDurationSeconds} -> segmentDurationSeconds) (\s@MssPackage' {} a -> s {segmentDurationSeconds = a} :: MssPackage)

-- | Undocumented member.
mssPackage_streamSelection :: Lens.Lens' MssPackage (Prelude.Maybe StreamSelection)
mssPackage_streamSelection = Lens.lens (\MssPackage' {streamSelection} -> streamSelection) (\s@MssPackage' {} a -> s {streamSelection = a} :: MssPackage)

-- | Undocumented member.
mssPackage_encryption :: Lens.Lens' MssPackage (Prelude.Maybe MssEncryption)
mssPackage_encryption = Lens.lens (\MssPackage' {encryption} -> encryption) (\s@MssPackage' {} a -> s {encryption = a} :: MssPackage)

-- | The time window (in seconds) contained in each manifest.
mssPackage_manifestWindowSeconds :: Lens.Lens' MssPackage (Prelude.Maybe Prelude.Int)
mssPackage_manifestWindowSeconds = Lens.lens (\MssPackage' {manifestWindowSeconds} -> manifestWindowSeconds) (\s@MssPackage' {} a -> s {manifestWindowSeconds = a} :: MssPackage)

instance Core.FromJSON MssPackage where
  parseJSON =
    Core.withObject
      "MssPackage"
      ( \x ->
          MssPackage'
            Prelude.<$> (x Core..:? "segmentDurationSeconds")
            Prelude.<*> (x Core..:? "streamSelection")
            Prelude.<*> (x Core..:? "encryption")
            Prelude.<*> (x Core..:? "manifestWindowSeconds")
      )

instance Prelude.Hashable MssPackage where
  hashWithSalt salt' MssPackage' {..} =
    salt' `Prelude.hashWithSalt` manifestWindowSeconds
      `Prelude.hashWithSalt` encryption
      `Prelude.hashWithSalt` streamSelection
      `Prelude.hashWithSalt` segmentDurationSeconds

instance Prelude.NFData MssPackage where
  rnf MssPackage' {..} =
    Prelude.rnf segmentDurationSeconds
      `Prelude.seq` Prelude.rnf manifestWindowSeconds
      `Prelude.seq` Prelude.rnf encryption
      `Prelude.seq` Prelude.rnf streamSelection

instance Core.ToJSON MssPackage where
  toJSON MssPackage' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("segmentDurationSeconds" Core..=)
              Prelude.<$> segmentDurationSeconds,
            ("streamSelection" Core..=)
              Prelude.<$> streamSelection,
            ("encryption" Core..=) Prelude.<$> encryption,
            ("manifestWindowSeconds" Core..=)
              Prelude.<$> manifestWindowSeconds
          ]
      )
