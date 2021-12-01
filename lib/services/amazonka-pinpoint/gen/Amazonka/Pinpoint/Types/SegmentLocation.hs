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
-- Module      : Amazonka.Pinpoint.Types.SegmentLocation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.SegmentLocation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.Pinpoint.Types.GPSPointDimension
import Amazonka.Pinpoint.Types.SetDimension
import qualified Amazonka.Prelude as Prelude

-- | Specifies geographical dimension settings for a segment.
--
-- /See:/ 'newSegmentLocation' smart constructor.
data SegmentLocation = SegmentLocation'
  { -- | The country or region code, in ISO 3166-1 alpha-2 format, for the
    -- segment.
    country :: Prelude.Maybe SetDimension,
    -- | The GPS location and range for the segment.
    gPSPoint :: Prelude.Maybe GPSPointDimension
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SegmentLocation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'country', 'segmentLocation_country' - The country or region code, in ISO 3166-1 alpha-2 format, for the
-- segment.
--
-- 'gPSPoint', 'segmentLocation_gPSPoint' - The GPS location and range for the segment.
newSegmentLocation ::
  SegmentLocation
newSegmentLocation =
  SegmentLocation'
    { country = Prelude.Nothing,
      gPSPoint = Prelude.Nothing
    }

-- | The country or region code, in ISO 3166-1 alpha-2 format, for the
-- segment.
segmentLocation_country :: Lens.Lens' SegmentLocation (Prelude.Maybe SetDimension)
segmentLocation_country = Lens.lens (\SegmentLocation' {country} -> country) (\s@SegmentLocation' {} a -> s {country = a} :: SegmentLocation)

-- | The GPS location and range for the segment.
segmentLocation_gPSPoint :: Lens.Lens' SegmentLocation (Prelude.Maybe GPSPointDimension)
segmentLocation_gPSPoint = Lens.lens (\SegmentLocation' {gPSPoint} -> gPSPoint) (\s@SegmentLocation' {} a -> s {gPSPoint = a} :: SegmentLocation)

instance Core.FromJSON SegmentLocation where
  parseJSON =
    Core.withObject
      "SegmentLocation"
      ( \x ->
          SegmentLocation'
            Prelude.<$> (x Core..:? "Country")
            Prelude.<*> (x Core..:? "GPSPoint")
      )

instance Prelude.Hashable SegmentLocation where
  hashWithSalt salt' SegmentLocation' {..} =
    salt' `Prelude.hashWithSalt` gPSPoint
      `Prelude.hashWithSalt` country

instance Prelude.NFData SegmentLocation where
  rnf SegmentLocation' {..} =
    Prelude.rnf country
      `Prelude.seq` Prelude.rnf gPSPoint

instance Core.ToJSON SegmentLocation where
  toJSON SegmentLocation' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Country" Core..=) Prelude.<$> country,
            ("GPSPoint" Core..=) Prelude.<$> gPSPoint
          ]
      )
