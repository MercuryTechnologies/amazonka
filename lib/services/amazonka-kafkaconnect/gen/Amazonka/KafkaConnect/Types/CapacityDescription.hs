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
-- Module      : Amazonka.KafkaConnect.Types.CapacityDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KafkaConnect.Types.CapacityDescription where

import qualified Amazonka.Core as Core
import Amazonka.KafkaConnect.Types.AutoScalingDescription
import Amazonka.KafkaConnect.Types.ProvisionedCapacityDescription
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | A description of the connector\'s capacity.
--
-- /See:/ 'newCapacityDescription' smart constructor.
data CapacityDescription = CapacityDescription'
  { -- | Describes the connector\'s auto scaling capacity.
    autoScaling :: Prelude.Maybe AutoScalingDescription,
    -- | Describes a connector\'s provisioned capacity.
    provisionedCapacity :: Prelude.Maybe ProvisionedCapacityDescription
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CapacityDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'autoScaling', 'capacityDescription_autoScaling' - Describes the connector\'s auto scaling capacity.
--
-- 'provisionedCapacity', 'capacityDescription_provisionedCapacity' - Describes a connector\'s provisioned capacity.
newCapacityDescription ::
  CapacityDescription
newCapacityDescription =
  CapacityDescription'
    { autoScaling = Prelude.Nothing,
      provisionedCapacity = Prelude.Nothing
    }

-- | Describes the connector\'s auto scaling capacity.
capacityDescription_autoScaling :: Lens.Lens' CapacityDescription (Prelude.Maybe AutoScalingDescription)
capacityDescription_autoScaling = Lens.lens (\CapacityDescription' {autoScaling} -> autoScaling) (\s@CapacityDescription' {} a -> s {autoScaling = a} :: CapacityDescription)

-- | Describes a connector\'s provisioned capacity.
capacityDescription_provisionedCapacity :: Lens.Lens' CapacityDescription (Prelude.Maybe ProvisionedCapacityDescription)
capacityDescription_provisionedCapacity = Lens.lens (\CapacityDescription' {provisionedCapacity} -> provisionedCapacity) (\s@CapacityDescription' {} a -> s {provisionedCapacity = a} :: CapacityDescription)

instance Core.FromJSON CapacityDescription where
  parseJSON =
    Core.withObject
      "CapacityDescription"
      ( \x ->
          CapacityDescription'
            Prelude.<$> (x Core..:? "autoScaling")
            Prelude.<*> (x Core..:? "provisionedCapacity")
      )

instance Prelude.Hashable CapacityDescription where
  hashWithSalt salt' CapacityDescription' {..} =
    salt' `Prelude.hashWithSalt` provisionedCapacity
      `Prelude.hashWithSalt` autoScaling

instance Prelude.NFData CapacityDescription where
  rnf CapacityDescription' {..} =
    Prelude.rnf autoScaling
      `Prelude.seq` Prelude.rnf provisionedCapacity
