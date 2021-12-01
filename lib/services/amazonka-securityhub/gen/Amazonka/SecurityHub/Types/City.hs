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
-- Module      : Amazonka.SecurityHub.Types.City
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.City where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information about a city.
--
-- /See:/ 'newCity' smart constructor.
data City = City'
  { -- | The name of the city.
    cityName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'City' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cityName', 'city_cityName' - The name of the city.
newCity ::
  City
newCity = City' {cityName = Prelude.Nothing}

-- | The name of the city.
city_cityName :: Lens.Lens' City (Prelude.Maybe Prelude.Text)
city_cityName = Lens.lens (\City' {cityName} -> cityName) (\s@City' {} a -> s {cityName = a} :: City)

instance Core.FromJSON City where
  parseJSON =
    Core.withObject
      "City"
      (\x -> City' Prelude.<$> (x Core..:? "CityName"))

instance Prelude.Hashable City where
  hashWithSalt salt' City' {..} =
    salt' `Prelude.hashWithSalt` cityName

instance Prelude.NFData City where
  rnf City' {..} = Prelude.rnf cityName

instance Core.ToJSON City where
  toJSON City' {..} =
    Core.object
      ( Prelude.catMaybes
          [("CityName" Core..=) Prelude.<$> cityName]
      )
