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
-- Module      : Amazonka.SecurityHub.Types.AwsEcsClusterDefaultCapacityProviderStrategyDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEcsClusterDefaultCapacityProviderStrategyDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The default capacity provider strategy for the cluster. The default
-- capacity provider strategy is used when services or tasks are run
-- without a specified launch type or capacity provider strategy.
--
-- /See:/ 'newAwsEcsClusterDefaultCapacityProviderStrategyDetails' smart constructor.
data AwsEcsClusterDefaultCapacityProviderStrategyDetails = AwsEcsClusterDefaultCapacityProviderStrategyDetails'
  { -- | The minimum number of tasks to run on the specified capacity provider.
    base :: Prelude.Maybe Prelude.Int,
    -- | The name of the capacity provider.
    capacityProvider :: Prelude.Maybe Prelude.Text,
    -- | The relative percentage of the total number of tasks launched that
    -- should use the capacity provider.
    weight :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsEcsClusterDefaultCapacityProviderStrategyDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'base', 'awsEcsClusterDefaultCapacityProviderStrategyDetails_base' - The minimum number of tasks to run on the specified capacity provider.
--
-- 'capacityProvider', 'awsEcsClusterDefaultCapacityProviderStrategyDetails_capacityProvider' - The name of the capacity provider.
--
-- 'weight', 'awsEcsClusterDefaultCapacityProviderStrategyDetails_weight' - The relative percentage of the total number of tasks launched that
-- should use the capacity provider.
newAwsEcsClusterDefaultCapacityProviderStrategyDetails ::
  AwsEcsClusterDefaultCapacityProviderStrategyDetails
newAwsEcsClusterDefaultCapacityProviderStrategyDetails =
  AwsEcsClusterDefaultCapacityProviderStrategyDetails'
    { base =
        Prelude.Nothing,
      capacityProvider =
        Prelude.Nothing,
      weight =
        Prelude.Nothing
    }

-- | The minimum number of tasks to run on the specified capacity provider.
awsEcsClusterDefaultCapacityProviderStrategyDetails_base :: Lens.Lens' AwsEcsClusterDefaultCapacityProviderStrategyDetails (Prelude.Maybe Prelude.Int)
awsEcsClusterDefaultCapacityProviderStrategyDetails_base = Lens.lens (\AwsEcsClusterDefaultCapacityProviderStrategyDetails' {base} -> base) (\s@AwsEcsClusterDefaultCapacityProviderStrategyDetails' {} a -> s {base = a} :: AwsEcsClusterDefaultCapacityProviderStrategyDetails)

-- | The name of the capacity provider.
awsEcsClusterDefaultCapacityProviderStrategyDetails_capacityProvider :: Lens.Lens' AwsEcsClusterDefaultCapacityProviderStrategyDetails (Prelude.Maybe Prelude.Text)
awsEcsClusterDefaultCapacityProviderStrategyDetails_capacityProvider = Lens.lens (\AwsEcsClusterDefaultCapacityProviderStrategyDetails' {capacityProvider} -> capacityProvider) (\s@AwsEcsClusterDefaultCapacityProviderStrategyDetails' {} a -> s {capacityProvider = a} :: AwsEcsClusterDefaultCapacityProviderStrategyDetails)

-- | The relative percentage of the total number of tasks launched that
-- should use the capacity provider.
awsEcsClusterDefaultCapacityProviderStrategyDetails_weight :: Lens.Lens' AwsEcsClusterDefaultCapacityProviderStrategyDetails (Prelude.Maybe Prelude.Int)
awsEcsClusterDefaultCapacityProviderStrategyDetails_weight = Lens.lens (\AwsEcsClusterDefaultCapacityProviderStrategyDetails' {weight} -> weight) (\s@AwsEcsClusterDefaultCapacityProviderStrategyDetails' {} a -> s {weight = a} :: AwsEcsClusterDefaultCapacityProviderStrategyDetails)

instance
  Core.FromJSON
    AwsEcsClusterDefaultCapacityProviderStrategyDetails
  where
  parseJSON =
    Core.withObject
      "AwsEcsClusterDefaultCapacityProviderStrategyDetails"
      ( \x ->
          AwsEcsClusterDefaultCapacityProviderStrategyDetails'
            Prelude.<$> (x Core..:? "Base")
              Prelude.<*> (x Core..:? "CapacityProvider")
              Prelude.<*> (x Core..:? "Weight")
      )

instance
  Prelude.Hashable
    AwsEcsClusterDefaultCapacityProviderStrategyDetails
  where
  hashWithSalt
    salt'
    AwsEcsClusterDefaultCapacityProviderStrategyDetails' {..} =
      salt' `Prelude.hashWithSalt` weight
        `Prelude.hashWithSalt` capacityProvider
        `Prelude.hashWithSalt` base

instance
  Prelude.NFData
    AwsEcsClusterDefaultCapacityProviderStrategyDetails
  where
  rnf
    AwsEcsClusterDefaultCapacityProviderStrategyDetails' {..} =
      Prelude.rnf base `Prelude.seq` Prelude.rnf weight
        `Prelude.seq` Prelude.rnf capacityProvider

instance
  Core.ToJSON
    AwsEcsClusterDefaultCapacityProviderStrategyDetails
  where
  toJSON
    AwsEcsClusterDefaultCapacityProviderStrategyDetails' {..} =
      Core.object
        ( Prelude.catMaybes
            [ ("Base" Core..=) Prelude.<$> base,
              ("CapacityProvider" Core..=)
                Prelude.<$> capacityProvider,
              ("Weight" Core..=) Prelude.<$> weight
            ]
        )
