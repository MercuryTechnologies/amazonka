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
-- Module      : Amazonka.GlobalAccelerator.Types.EndpointGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GlobalAccelerator.Types.EndpointGroup where

import qualified Amazonka.Core as Core
import Amazonka.GlobalAccelerator.Types.EndpointDescription
import Amazonka.GlobalAccelerator.Types.HealthCheckProtocol
import Amazonka.GlobalAccelerator.Types.PortOverride
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | A complex type for the endpoint group. An AWS Region can have only one
-- endpoint group for a specific listener.
--
-- /See:/ 'newEndpointGroup' smart constructor.
data EndpointGroup = EndpointGroup'
  { -- | The number of consecutive health checks required to set the state of a
    -- healthy endpoint to unhealthy, or to set an unhealthy endpoint to
    -- healthy. The default value is 3.
    thresholdCount :: Prelude.Maybe Prelude.Natural,
    -- | If the protocol is HTTP\/S, then this value provides the ping path that
    -- Global Accelerator uses for the destination on the endpoints for health
    -- checks. The default is slash (\/).
    healthCheckPath :: Prelude.Maybe Prelude.Text,
    -- | The time—10 seconds or 30 seconds—between health checks for each
    -- endpoint. The default value is 30.
    healthCheckIntervalSeconds :: Prelude.Maybe Prelude.Natural,
    -- | The Amazon Resource Name (ARN) of the endpoint group.
    endpointGroupArn :: Prelude.Maybe Prelude.Text,
    -- | The protocol that Global Accelerator uses to perform health checks on
    -- endpoints that are part of this endpoint group. The default value is
    -- TCP.
    healthCheckProtocol :: Prelude.Maybe HealthCheckProtocol,
    -- | The AWS Region where the endpoint group is located.
    endpointGroupRegion :: Prelude.Maybe Prelude.Text,
    -- | The percentage of traffic to send to an AWS Region. Additional traffic
    -- is distributed to other endpoint groups for this listener.
    --
    -- Use this action to increase (dial up) or decrease (dial down) traffic to
    -- a specific Region. The percentage is applied to the traffic that would
    -- otherwise have been routed to the Region based on optimal routing.
    --
    -- The default value is 100.
    trafficDialPercentage :: Prelude.Maybe Prelude.Double,
    -- | The port that Global Accelerator uses to perform health checks on
    -- endpoints that are part of this endpoint group.
    --
    -- The default port is the port for the listener that this endpoint group
    -- is associated with. If the listener port is a list, Global Accelerator
    -- uses the first specified port in the list of ports.
    healthCheckPort :: Prelude.Maybe Prelude.Natural,
    -- | Allows you to override the destination ports used to route traffic to an
    -- endpoint. Using a port override lets you to map a list of external
    -- destination ports (that your users send traffic to) to a list of
    -- internal destination ports that you want an application endpoint to
    -- receive traffic on.
    portOverrides :: Prelude.Maybe [PortOverride],
    -- | The list of endpoint objects.
    endpointDescriptions :: Prelude.Maybe [EndpointDescription]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EndpointGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'thresholdCount', 'endpointGroup_thresholdCount' - The number of consecutive health checks required to set the state of a
-- healthy endpoint to unhealthy, or to set an unhealthy endpoint to
-- healthy. The default value is 3.
--
-- 'healthCheckPath', 'endpointGroup_healthCheckPath' - If the protocol is HTTP\/S, then this value provides the ping path that
-- Global Accelerator uses for the destination on the endpoints for health
-- checks. The default is slash (\/).
--
-- 'healthCheckIntervalSeconds', 'endpointGroup_healthCheckIntervalSeconds' - The time—10 seconds or 30 seconds—between health checks for each
-- endpoint. The default value is 30.
--
-- 'endpointGroupArn', 'endpointGroup_endpointGroupArn' - The Amazon Resource Name (ARN) of the endpoint group.
--
-- 'healthCheckProtocol', 'endpointGroup_healthCheckProtocol' - The protocol that Global Accelerator uses to perform health checks on
-- endpoints that are part of this endpoint group. The default value is
-- TCP.
--
-- 'endpointGroupRegion', 'endpointGroup_endpointGroupRegion' - The AWS Region where the endpoint group is located.
--
-- 'trafficDialPercentage', 'endpointGroup_trafficDialPercentage' - The percentage of traffic to send to an AWS Region. Additional traffic
-- is distributed to other endpoint groups for this listener.
--
-- Use this action to increase (dial up) or decrease (dial down) traffic to
-- a specific Region. The percentage is applied to the traffic that would
-- otherwise have been routed to the Region based on optimal routing.
--
-- The default value is 100.
--
-- 'healthCheckPort', 'endpointGroup_healthCheckPort' - The port that Global Accelerator uses to perform health checks on
-- endpoints that are part of this endpoint group.
--
-- The default port is the port for the listener that this endpoint group
-- is associated with. If the listener port is a list, Global Accelerator
-- uses the first specified port in the list of ports.
--
-- 'portOverrides', 'endpointGroup_portOverrides' - Allows you to override the destination ports used to route traffic to an
-- endpoint. Using a port override lets you to map a list of external
-- destination ports (that your users send traffic to) to a list of
-- internal destination ports that you want an application endpoint to
-- receive traffic on.
--
-- 'endpointDescriptions', 'endpointGroup_endpointDescriptions' - The list of endpoint objects.
newEndpointGroup ::
  EndpointGroup
newEndpointGroup =
  EndpointGroup'
    { thresholdCount = Prelude.Nothing,
      healthCheckPath = Prelude.Nothing,
      healthCheckIntervalSeconds = Prelude.Nothing,
      endpointGroupArn = Prelude.Nothing,
      healthCheckProtocol = Prelude.Nothing,
      endpointGroupRegion = Prelude.Nothing,
      trafficDialPercentage = Prelude.Nothing,
      healthCheckPort = Prelude.Nothing,
      portOverrides = Prelude.Nothing,
      endpointDescriptions = Prelude.Nothing
    }

-- | The number of consecutive health checks required to set the state of a
-- healthy endpoint to unhealthy, or to set an unhealthy endpoint to
-- healthy. The default value is 3.
endpointGroup_thresholdCount :: Lens.Lens' EndpointGroup (Prelude.Maybe Prelude.Natural)
endpointGroup_thresholdCount = Lens.lens (\EndpointGroup' {thresholdCount} -> thresholdCount) (\s@EndpointGroup' {} a -> s {thresholdCount = a} :: EndpointGroup)

-- | If the protocol is HTTP\/S, then this value provides the ping path that
-- Global Accelerator uses for the destination on the endpoints for health
-- checks. The default is slash (\/).
endpointGroup_healthCheckPath :: Lens.Lens' EndpointGroup (Prelude.Maybe Prelude.Text)
endpointGroup_healthCheckPath = Lens.lens (\EndpointGroup' {healthCheckPath} -> healthCheckPath) (\s@EndpointGroup' {} a -> s {healthCheckPath = a} :: EndpointGroup)

-- | The time—10 seconds or 30 seconds—between health checks for each
-- endpoint. The default value is 30.
endpointGroup_healthCheckIntervalSeconds :: Lens.Lens' EndpointGroup (Prelude.Maybe Prelude.Natural)
endpointGroup_healthCheckIntervalSeconds = Lens.lens (\EndpointGroup' {healthCheckIntervalSeconds} -> healthCheckIntervalSeconds) (\s@EndpointGroup' {} a -> s {healthCheckIntervalSeconds = a} :: EndpointGroup)

-- | The Amazon Resource Name (ARN) of the endpoint group.
endpointGroup_endpointGroupArn :: Lens.Lens' EndpointGroup (Prelude.Maybe Prelude.Text)
endpointGroup_endpointGroupArn = Lens.lens (\EndpointGroup' {endpointGroupArn} -> endpointGroupArn) (\s@EndpointGroup' {} a -> s {endpointGroupArn = a} :: EndpointGroup)

-- | The protocol that Global Accelerator uses to perform health checks on
-- endpoints that are part of this endpoint group. The default value is
-- TCP.
endpointGroup_healthCheckProtocol :: Lens.Lens' EndpointGroup (Prelude.Maybe HealthCheckProtocol)
endpointGroup_healthCheckProtocol = Lens.lens (\EndpointGroup' {healthCheckProtocol} -> healthCheckProtocol) (\s@EndpointGroup' {} a -> s {healthCheckProtocol = a} :: EndpointGroup)

-- | The AWS Region where the endpoint group is located.
endpointGroup_endpointGroupRegion :: Lens.Lens' EndpointGroup (Prelude.Maybe Prelude.Text)
endpointGroup_endpointGroupRegion = Lens.lens (\EndpointGroup' {endpointGroupRegion} -> endpointGroupRegion) (\s@EndpointGroup' {} a -> s {endpointGroupRegion = a} :: EndpointGroup)

-- | The percentage of traffic to send to an AWS Region. Additional traffic
-- is distributed to other endpoint groups for this listener.
--
-- Use this action to increase (dial up) or decrease (dial down) traffic to
-- a specific Region. The percentage is applied to the traffic that would
-- otherwise have been routed to the Region based on optimal routing.
--
-- The default value is 100.
endpointGroup_trafficDialPercentage :: Lens.Lens' EndpointGroup (Prelude.Maybe Prelude.Double)
endpointGroup_trafficDialPercentage = Lens.lens (\EndpointGroup' {trafficDialPercentage} -> trafficDialPercentage) (\s@EndpointGroup' {} a -> s {trafficDialPercentage = a} :: EndpointGroup)

-- | The port that Global Accelerator uses to perform health checks on
-- endpoints that are part of this endpoint group.
--
-- The default port is the port for the listener that this endpoint group
-- is associated with. If the listener port is a list, Global Accelerator
-- uses the first specified port in the list of ports.
endpointGroup_healthCheckPort :: Lens.Lens' EndpointGroup (Prelude.Maybe Prelude.Natural)
endpointGroup_healthCheckPort = Lens.lens (\EndpointGroup' {healthCheckPort} -> healthCheckPort) (\s@EndpointGroup' {} a -> s {healthCheckPort = a} :: EndpointGroup)

-- | Allows you to override the destination ports used to route traffic to an
-- endpoint. Using a port override lets you to map a list of external
-- destination ports (that your users send traffic to) to a list of
-- internal destination ports that you want an application endpoint to
-- receive traffic on.
endpointGroup_portOverrides :: Lens.Lens' EndpointGroup (Prelude.Maybe [PortOverride])
endpointGroup_portOverrides = Lens.lens (\EndpointGroup' {portOverrides} -> portOverrides) (\s@EndpointGroup' {} a -> s {portOverrides = a} :: EndpointGroup) Prelude.. Lens.mapping Lens.coerced

-- | The list of endpoint objects.
endpointGroup_endpointDescriptions :: Lens.Lens' EndpointGroup (Prelude.Maybe [EndpointDescription])
endpointGroup_endpointDescriptions = Lens.lens (\EndpointGroup' {endpointDescriptions} -> endpointDescriptions) (\s@EndpointGroup' {} a -> s {endpointDescriptions = a} :: EndpointGroup) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON EndpointGroup where
  parseJSON =
    Core.withObject
      "EndpointGroup"
      ( \x ->
          EndpointGroup'
            Prelude.<$> (x Core..:? "ThresholdCount")
            Prelude.<*> (x Core..:? "HealthCheckPath")
            Prelude.<*> (x Core..:? "HealthCheckIntervalSeconds")
            Prelude.<*> (x Core..:? "EndpointGroupArn")
            Prelude.<*> (x Core..:? "HealthCheckProtocol")
            Prelude.<*> (x Core..:? "EndpointGroupRegion")
            Prelude.<*> (x Core..:? "TrafficDialPercentage")
            Prelude.<*> (x Core..:? "HealthCheckPort")
            Prelude.<*> (x Core..:? "PortOverrides" Core..!= Prelude.mempty)
            Prelude.<*> ( x Core..:? "EndpointDescriptions"
                            Core..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable EndpointGroup where
  hashWithSalt salt' EndpointGroup' {..} =
    salt' `Prelude.hashWithSalt` endpointDescriptions
      `Prelude.hashWithSalt` portOverrides
      `Prelude.hashWithSalt` healthCheckPort
      `Prelude.hashWithSalt` trafficDialPercentage
      `Prelude.hashWithSalt` endpointGroupRegion
      `Prelude.hashWithSalt` healthCheckProtocol
      `Prelude.hashWithSalt` endpointGroupArn
      `Prelude.hashWithSalt` healthCheckIntervalSeconds
      `Prelude.hashWithSalt` healthCheckPath
      `Prelude.hashWithSalt` thresholdCount

instance Prelude.NFData EndpointGroup where
  rnf EndpointGroup' {..} =
    Prelude.rnf thresholdCount
      `Prelude.seq` Prelude.rnf endpointDescriptions
      `Prelude.seq` Prelude.rnf portOverrides
      `Prelude.seq` Prelude.rnf healthCheckPort
      `Prelude.seq` Prelude.rnf trafficDialPercentage
      `Prelude.seq` Prelude.rnf endpointGroupRegion
      `Prelude.seq` Prelude.rnf healthCheckProtocol
      `Prelude.seq` Prelude.rnf endpointGroupArn
      `Prelude.seq` Prelude.rnf healthCheckIntervalSeconds
      `Prelude.seq` Prelude.rnf healthCheckPath
