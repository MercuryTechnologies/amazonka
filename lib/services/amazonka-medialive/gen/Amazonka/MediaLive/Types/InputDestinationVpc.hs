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
-- Module      : Amazonka.MediaLive.Types.InputDestinationVpc
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaLive.Types.InputDestinationVpc where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The properties for a VPC type input destination.
--
-- /See:/ 'newInputDestinationVpc' smart constructor.
data InputDestinationVpc = InputDestinationVpc'
  { -- | The network interface ID of the Input destination in the VPC.
    networkInterfaceId :: Prelude.Maybe Prelude.Text,
    -- | The availability zone of the Input destination.
    availabilityZone :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InputDestinationVpc' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'networkInterfaceId', 'inputDestinationVpc_networkInterfaceId' - The network interface ID of the Input destination in the VPC.
--
-- 'availabilityZone', 'inputDestinationVpc_availabilityZone' - The availability zone of the Input destination.
newInputDestinationVpc ::
  InputDestinationVpc
newInputDestinationVpc =
  InputDestinationVpc'
    { networkInterfaceId =
        Prelude.Nothing,
      availabilityZone = Prelude.Nothing
    }

-- | The network interface ID of the Input destination in the VPC.
inputDestinationVpc_networkInterfaceId :: Lens.Lens' InputDestinationVpc (Prelude.Maybe Prelude.Text)
inputDestinationVpc_networkInterfaceId = Lens.lens (\InputDestinationVpc' {networkInterfaceId} -> networkInterfaceId) (\s@InputDestinationVpc' {} a -> s {networkInterfaceId = a} :: InputDestinationVpc)

-- | The availability zone of the Input destination.
inputDestinationVpc_availabilityZone :: Lens.Lens' InputDestinationVpc (Prelude.Maybe Prelude.Text)
inputDestinationVpc_availabilityZone = Lens.lens (\InputDestinationVpc' {availabilityZone} -> availabilityZone) (\s@InputDestinationVpc' {} a -> s {availabilityZone = a} :: InputDestinationVpc)

instance Core.FromJSON InputDestinationVpc where
  parseJSON =
    Core.withObject
      "InputDestinationVpc"
      ( \x ->
          InputDestinationVpc'
            Prelude.<$> (x Core..:? "networkInterfaceId")
            Prelude.<*> (x Core..:? "availabilityZone")
      )

instance Prelude.Hashable InputDestinationVpc where
  hashWithSalt salt' InputDestinationVpc' {..} =
    salt' `Prelude.hashWithSalt` availabilityZone
      `Prelude.hashWithSalt` networkInterfaceId

instance Prelude.NFData InputDestinationVpc where
  rnf InputDestinationVpc' {..} =
    Prelude.rnf networkInterfaceId
      `Prelude.seq` Prelude.rnf availabilityZone
