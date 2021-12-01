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
-- Module      : Amazonka.GameLift.Types.LocationConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GameLift.Types.LocationConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | A remote location where a multi-location fleet can deploy EC2 instances
-- for game hosting.
--
-- __Related actions__
--
-- CreateFleet
--
-- /See:/ 'newLocationConfiguration' smart constructor.
data LocationConfiguration = LocationConfiguration'
  { -- | An AWS Region code, such as @us-west-2@.
    location :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LocationConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'location', 'locationConfiguration_location' - An AWS Region code, such as @us-west-2@.
newLocationConfiguration ::
  LocationConfiguration
newLocationConfiguration =
  LocationConfiguration' {location = Prelude.Nothing}

-- | An AWS Region code, such as @us-west-2@.
locationConfiguration_location :: Lens.Lens' LocationConfiguration (Prelude.Maybe Prelude.Text)
locationConfiguration_location = Lens.lens (\LocationConfiguration' {location} -> location) (\s@LocationConfiguration' {} a -> s {location = a} :: LocationConfiguration)

instance Prelude.Hashable LocationConfiguration where
  hashWithSalt salt' LocationConfiguration' {..} =
    salt' `Prelude.hashWithSalt` location

instance Prelude.NFData LocationConfiguration where
  rnf LocationConfiguration' {..} = Prelude.rnf location

instance Core.ToJSON LocationConfiguration where
  toJSON LocationConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [("Location" Core..=) Prelude.<$> location]
      )
