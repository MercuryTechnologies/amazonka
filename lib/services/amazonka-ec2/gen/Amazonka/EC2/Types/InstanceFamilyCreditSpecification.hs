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
-- Module      : Amazonka.EC2.Types.InstanceFamilyCreditSpecification
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.InstanceFamilyCreditSpecification where

import qualified Amazonka.Core as Core
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.UnlimitedSupportedInstanceFamily
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes the default credit option for CPU usage of a burstable
-- performance instance family.
--
-- /See:/ 'newInstanceFamilyCreditSpecification' smart constructor.
data InstanceFamilyCreditSpecification = InstanceFamilyCreditSpecification'
  { -- | The instance family.
    instanceFamily :: Prelude.Maybe UnlimitedSupportedInstanceFamily,
    -- | The default credit option for CPU usage of the instance family. Valid
    -- values are @standard@ and @unlimited@.
    cpuCredits :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InstanceFamilyCreditSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceFamily', 'instanceFamilyCreditSpecification_instanceFamily' - The instance family.
--
-- 'cpuCredits', 'instanceFamilyCreditSpecification_cpuCredits' - The default credit option for CPU usage of the instance family. Valid
-- values are @standard@ and @unlimited@.
newInstanceFamilyCreditSpecification ::
  InstanceFamilyCreditSpecification
newInstanceFamilyCreditSpecification =
  InstanceFamilyCreditSpecification'
    { instanceFamily =
        Prelude.Nothing,
      cpuCredits = Prelude.Nothing
    }

-- | The instance family.
instanceFamilyCreditSpecification_instanceFamily :: Lens.Lens' InstanceFamilyCreditSpecification (Prelude.Maybe UnlimitedSupportedInstanceFamily)
instanceFamilyCreditSpecification_instanceFamily = Lens.lens (\InstanceFamilyCreditSpecification' {instanceFamily} -> instanceFamily) (\s@InstanceFamilyCreditSpecification' {} a -> s {instanceFamily = a} :: InstanceFamilyCreditSpecification)

-- | The default credit option for CPU usage of the instance family. Valid
-- values are @standard@ and @unlimited@.
instanceFamilyCreditSpecification_cpuCredits :: Lens.Lens' InstanceFamilyCreditSpecification (Prelude.Maybe Prelude.Text)
instanceFamilyCreditSpecification_cpuCredits = Lens.lens (\InstanceFamilyCreditSpecification' {cpuCredits} -> cpuCredits) (\s@InstanceFamilyCreditSpecification' {} a -> s {cpuCredits = a} :: InstanceFamilyCreditSpecification)

instance
  Core.FromXML
    InstanceFamilyCreditSpecification
  where
  parseXML x =
    InstanceFamilyCreditSpecification'
      Prelude.<$> (x Core..@? "instanceFamily")
      Prelude.<*> (x Core..@? "cpuCredits")

instance
  Prelude.Hashable
    InstanceFamilyCreditSpecification
  where
  hashWithSalt
    salt'
    InstanceFamilyCreditSpecification' {..} =
      salt' `Prelude.hashWithSalt` cpuCredits
        `Prelude.hashWithSalt` instanceFamily

instance
  Prelude.NFData
    InstanceFamilyCreditSpecification
  where
  rnf InstanceFamilyCreditSpecification' {..} =
    Prelude.rnf instanceFamily
      `Prelude.seq` Prelude.rnf cpuCredits
