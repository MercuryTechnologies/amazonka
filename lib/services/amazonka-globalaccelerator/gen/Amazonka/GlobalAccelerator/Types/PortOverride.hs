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
-- Module      : Amazonka.GlobalAccelerator.Types.PortOverride
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GlobalAccelerator.Types.PortOverride where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Override specific listener ports used to route traffic to endpoints that
-- are part of an endpoint group. For example, you can create a port
-- override in which the listener receives user traffic on ports 80 and
-- 443, but your accelerator routes that traffic to ports 1080 and 1443,
-- respectively, on the endpoints.
--
-- For more information, see
-- <https://docs.aws.amazon.com/global-accelerator/latest/dg/about-endpoint-groups-port-override.html Port overrides>
-- in the /AWS Global Accelerator Developer Guide/.
--
-- /See:/ 'newPortOverride' smart constructor.
data PortOverride = PortOverride'
  { -- | The endpoint port that you want a listener port to be mapped to. This is
    -- the port on the endpoint, such as the Application Load Balancer or
    -- Amazon EC2 instance.
    endpointPort :: Prelude.Maybe Prelude.Natural,
    -- | The listener port that you want to map to a specific endpoint port. This
    -- is the port that user traffic arrives to the Global Accelerator on.
    listenerPort :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PortOverride' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endpointPort', 'portOverride_endpointPort' - The endpoint port that you want a listener port to be mapped to. This is
-- the port on the endpoint, such as the Application Load Balancer or
-- Amazon EC2 instance.
--
-- 'listenerPort', 'portOverride_listenerPort' - The listener port that you want to map to a specific endpoint port. This
-- is the port that user traffic arrives to the Global Accelerator on.
newPortOverride ::
  PortOverride
newPortOverride =
  PortOverride'
    { endpointPort = Prelude.Nothing,
      listenerPort = Prelude.Nothing
    }

-- | The endpoint port that you want a listener port to be mapped to. This is
-- the port on the endpoint, such as the Application Load Balancer or
-- Amazon EC2 instance.
portOverride_endpointPort :: Lens.Lens' PortOverride (Prelude.Maybe Prelude.Natural)
portOverride_endpointPort = Lens.lens (\PortOverride' {endpointPort} -> endpointPort) (\s@PortOverride' {} a -> s {endpointPort = a} :: PortOverride)

-- | The listener port that you want to map to a specific endpoint port. This
-- is the port that user traffic arrives to the Global Accelerator on.
portOverride_listenerPort :: Lens.Lens' PortOverride (Prelude.Maybe Prelude.Natural)
portOverride_listenerPort = Lens.lens (\PortOverride' {listenerPort} -> listenerPort) (\s@PortOverride' {} a -> s {listenerPort = a} :: PortOverride)

instance Core.FromJSON PortOverride where
  parseJSON =
    Core.withObject
      "PortOverride"
      ( \x ->
          PortOverride'
            Prelude.<$> (x Core..:? "EndpointPort")
            Prelude.<*> (x Core..:? "ListenerPort")
      )

instance Prelude.Hashable PortOverride where
  hashWithSalt salt' PortOverride' {..} =
    salt' `Prelude.hashWithSalt` listenerPort
      `Prelude.hashWithSalt` endpointPort

instance Prelude.NFData PortOverride where
  rnf PortOverride' {..} =
    Prelude.rnf endpointPort
      `Prelude.seq` Prelude.rnf listenerPort

instance Core.ToJSON PortOverride where
  toJSON PortOverride' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("EndpointPort" Core..=) Prelude.<$> endpointPort,
            ("ListenerPort" Core..=) Prelude.<$> listenerPort
          ]
      )
