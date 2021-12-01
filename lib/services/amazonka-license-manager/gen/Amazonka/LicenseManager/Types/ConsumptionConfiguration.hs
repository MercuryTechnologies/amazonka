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
-- Module      : Amazonka.LicenseManager.Types.ConsumptionConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LicenseManager.Types.ConsumptionConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.LicenseManager.Types.BorrowConfiguration
import Amazonka.LicenseManager.Types.ProvisionalConfiguration
import Amazonka.LicenseManager.Types.RenewType
import qualified Amazonka.Prelude as Prelude

-- | Details about a consumption configuration.
--
-- /See:/ 'newConsumptionConfiguration' smart constructor.
data ConsumptionConfiguration = ConsumptionConfiguration'
  { -- | Details about a borrow configuration.
    borrowConfiguration :: Prelude.Maybe BorrowConfiguration,
    -- | Details about a provisional configuration.
    provisionalConfiguration :: Prelude.Maybe ProvisionalConfiguration,
    -- | Renewal frequency.
    renewType :: Prelude.Maybe RenewType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConsumptionConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'borrowConfiguration', 'consumptionConfiguration_borrowConfiguration' - Details about a borrow configuration.
--
-- 'provisionalConfiguration', 'consumptionConfiguration_provisionalConfiguration' - Details about a provisional configuration.
--
-- 'renewType', 'consumptionConfiguration_renewType' - Renewal frequency.
newConsumptionConfiguration ::
  ConsumptionConfiguration
newConsumptionConfiguration =
  ConsumptionConfiguration'
    { borrowConfiguration =
        Prelude.Nothing,
      provisionalConfiguration = Prelude.Nothing,
      renewType = Prelude.Nothing
    }

-- | Details about a borrow configuration.
consumptionConfiguration_borrowConfiguration :: Lens.Lens' ConsumptionConfiguration (Prelude.Maybe BorrowConfiguration)
consumptionConfiguration_borrowConfiguration = Lens.lens (\ConsumptionConfiguration' {borrowConfiguration} -> borrowConfiguration) (\s@ConsumptionConfiguration' {} a -> s {borrowConfiguration = a} :: ConsumptionConfiguration)

-- | Details about a provisional configuration.
consumptionConfiguration_provisionalConfiguration :: Lens.Lens' ConsumptionConfiguration (Prelude.Maybe ProvisionalConfiguration)
consumptionConfiguration_provisionalConfiguration = Lens.lens (\ConsumptionConfiguration' {provisionalConfiguration} -> provisionalConfiguration) (\s@ConsumptionConfiguration' {} a -> s {provisionalConfiguration = a} :: ConsumptionConfiguration)

-- | Renewal frequency.
consumptionConfiguration_renewType :: Lens.Lens' ConsumptionConfiguration (Prelude.Maybe RenewType)
consumptionConfiguration_renewType = Lens.lens (\ConsumptionConfiguration' {renewType} -> renewType) (\s@ConsumptionConfiguration' {} a -> s {renewType = a} :: ConsumptionConfiguration)

instance Core.FromJSON ConsumptionConfiguration where
  parseJSON =
    Core.withObject
      "ConsumptionConfiguration"
      ( \x ->
          ConsumptionConfiguration'
            Prelude.<$> (x Core..:? "BorrowConfiguration")
            Prelude.<*> (x Core..:? "ProvisionalConfiguration")
            Prelude.<*> (x Core..:? "RenewType")
      )

instance Prelude.Hashable ConsumptionConfiguration where
  hashWithSalt salt' ConsumptionConfiguration' {..} =
    salt' `Prelude.hashWithSalt` renewType
      `Prelude.hashWithSalt` provisionalConfiguration
      `Prelude.hashWithSalt` borrowConfiguration

instance Prelude.NFData ConsumptionConfiguration where
  rnf ConsumptionConfiguration' {..} =
    Prelude.rnf borrowConfiguration
      `Prelude.seq` Prelude.rnf renewType
      `Prelude.seq` Prelude.rnf provisionalConfiguration

instance Core.ToJSON ConsumptionConfiguration where
  toJSON ConsumptionConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("BorrowConfiguration" Core..=)
              Prelude.<$> borrowConfiguration,
            ("ProvisionalConfiguration" Core..=)
              Prelude.<$> provisionalConfiguration,
            ("RenewType" Core..=) Prelude.<$> renewType
          ]
      )
