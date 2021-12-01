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
-- Module      : Amazonka.SecurityHub.Types.GeoLocation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.GeoLocation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Provides the latitude and longitude coordinates of a location.
--
-- /See:/ 'newGeoLocation' smart constructor.
data GeoLocation = GeoLocation'
  { -- | The latitude of the location.
    lat :: Prelude.Maybe Prelude.Double,
    -- | The longitude of the location.
    lon :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GeoLocation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lat', 'geoLocation_lat' - The latitude of the location.
--
-- 'lon', 'geoLocation_lon' - The longitude of the location.
newGeoLocation ::
  GeoLocation
newGeoLocation =
  GeoLocation'
    { lat = Prelude.Nothing,
      lon = Prelude.Nothing
    }

-- | The latitude of the location.
geoLocation_lat :: Lens.Lens' GeoLocation (Prelude.Maybe Prelude.Double)
geoLocation_lat = Lens.lens (\GeoLocation' {lat} -> lat) (\s@GeoLocation' {} a -> s {lat = a} :: GeoLocation)

-- | The longitude of the location.
geoLocation_lon :: Lens.Lens' GeoLocation (Prelude.Maybe Prelude.Double)
geoLocation_lon = Lens.lens (\GeoLocation' {lon} -> lon) (\s@GeoLocation' {} a -> s {lon = a} :: GeoLocation)

instance Core.FromJSON GeoLocation where
  parseJSON =
    Core.withObject
      "GeoLocation"
      ( \x ->
          GeoLocation'
            Prelude.<$> (x Core..:? "Lat") Prelude.<*> (x Core..:? "Lon")
      )

instance Prelude.Hashable GeoLocation where
  hashWithSalt salt' GeoLocation' {..} =
    salt' `Prelude.hashWithSalt` lon
      `Prelude.hashWithSalt` lat

instance Prelude.NFData GeoLocation where
  rnf GeoLocation' {..} =
    Prelude.rnf lat `Prelude.seq` Prelude.rnf lon

instance Core.ToJSON GeoLocation where
  toJSON GeoLocation' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Lat" Core..=) Prelude.<$> lat,
            ("Lon" Core..=) Prelude.<$> lon
          ]
      )
