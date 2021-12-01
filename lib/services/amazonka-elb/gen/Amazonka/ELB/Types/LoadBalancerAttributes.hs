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
-- Module      : Amazonka.ELB.Types.LoadBalancerAttributes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ELB.Types.LoadBalancerAttributes where

import qualified Amazonka.Core as Core
import Amazonka.ELB.Internal
import Amazonka.ELB.Types.AccessLog
import Amazonka.ELB.Types.AdditionalAttribute
import Amazonka.ELB.Types.ConnectionDraining
import Amazonka.ELB.Types.ConnectionSettings
import Amazonka.ELB.Types.CrossZoneLoadBalancing
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The attributes for a load balancer.
--
-- /See:/ 'newLoadBalancerAttributes' smart constructor.
data LoadBalancerAttributes = LoadBalancerAttributes'
  { -- | If enabled, the load balancer routes the request traffic evenly across
    -- all instances regardless of the Availability Zones.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/elasticloadbalancing/latest/classic/enable-disable-crosszone-lb.html Configure Cross-Zone Load Balancing>
    -- in the /Classic Load Balancers Guide/.
    crossZoneLoadBalancing :: Prelude.Maybe CrossZoneLoadBalancing,
    -- | If enabled, the load balancer captures detailed information of all
    -- requests and delivers the information to the Amazon S3 bucket that you
    -- specify.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/elasticloadbalancing/latest/classic/enable-access-logs.html Enable Access Logs>
    -- in the /Classic Load Balancers Guide/.
    accessLog :: Prelude.Maybe AccessLog,
    -- | Any additional attributes.
    additionalAttributes :: Prelude.Maybe [AdditionalAttribute],
    -- | If enabled, the load balancer allows the connections to remain idle (no
    -- data is sent over the connection) for the specified duration.
    --
    -- By default, Elastic Load Balancing maintains a 60-second idle connection
    -- timeout for both front-end and back-end connections of your load
    -- balancer. For more information, see
    -- <https://docs.aws.amazon.com/elasticloadbalancing/latest/classic/config-idle-timeout.html Configure Idle Connection Timeout>
    -- in the /Classic Load Balancers Guide/.
    connectionSettings :: Prelude.Maybe ConnectionSettings,
    -- | If enabled, the load balancer allows existing requests to complete
    -- before the load balancer shifts traffic away from a deregistered or
    -- unhealthy instance.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/elasticloadbalancing/latest/classic/config-conn-drain.html Configure Connection Draining>
    -- in the /Classic Load Balancers Guide/.
    connectionDraining :: Prelude.Maybe ConnectionDraining
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LoadBalancerAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'crossZoneLoadBalancing', 'loadBalancerAttributes_crossZoneLoadBalancing' - If enabled, the load balancer routes the request traffic evenly across
-- all instances regardless of the Availability Zones.
--
-- For more information, see
-- <https://docs.aws.amazon.com/elasticloadbalancing/latest/classic/enable-disable-crosszone-lb.html Configure Cross-Zone Load Balancing>
-- in the /Classic Load Balancers Guide/.
--
-- 'accessLog', 'loadBalancerAttributes_accessLog' - If enabled, the load balancer captures detailed information of all
-- requests and delivers the information to the Amazon S3 bucket that you
-- specify.
--
-- For more information, see
-- <https://docs.aws.amazon.com/elasticloadbalancing/latest/classic/enable-access-logs.html Enable Access Logs>
-- in the /Classic Load Balancers Guide/.
--
-- 'additionalAttributes', 'loadBalancerAttributes_additionalAttributes' - Any additional attributes.
--
-- 'connectionSettings', 'loadBalancerAttributes_connectionSettings' - If enabled, the load balancer allows the connections to remain idle (no
-- data is sent over the connection) for the specified duration.
--
-- By default, Elastic Load Balancing maintains a 60-second idle connection
-- timeout for both front-end and back-end connections of your load
-- balancer. For more information, see
-- <https://docs.aws.amazon.com/elasticloadbalancing/latest/classic/config-idle-timeout.html Configure Idle Connection Timeout>
-- in the /Classic Load Balancers Guide/.
--
-- 'connectionDraining', 'loadBalancerAttributes_connectionDraining' - If enabled, the load balancer allows existing requests to complete
-- before the load balancer shifts traffic away from a deregistered or
-- unhealthy instance.
--
-- For more information, see
-- <https://docs.aws.amazon.com/elasticloadbalancing/latest/classic/config-conn-drain.html Configure Connection Draining>
-- in the /Classic Load Balancers Guide/.
newLoadBalancerAttributes ::
  LoadBalancerAttributes
newLoadBalancerAttributes =
  LoadBalancerAttributes'
    { crossZoneLoadBalancing =
        Prelude.Nothing,
      accessLog = Prelude.Nothing,
      additionalAttributes = Prelude.Nothing,
      connectionSettings = Prelude.Nothing,
      connectionDraining = Prelude.Nothing
    }

-- | If enabled, the load balancer routes the request traffic evenly across
-- all instances regardless of the Availability Zones.
--
-- For more information, see
-- <https://docs.aws.amazon.com/elasticloadbalancing/latest/classic/enable-disable-crosszone-lb.html Configure Cross-Zone Load Balancing>
-- in the /Classic Load Balancers Guide/.
loadBalancerAttributes_crossZoneLoadBalancing :: Lens.Lens' LoadBalancerAttributes (Prelude.Maybe CrossZoneLoadBalancing)
loadBalancerAttributes_crossZoneLoadBalancing = Lens.lens (\LoadBalancerAttributes' {crossZoneLoadBalancing} -> crossZoneLoadBalancing) (\s@LoadBalancerAttributes' {} a -> s {crossZoneLoadBalancing = a} :: LoadBalancerAttributes)

-- | If enabled, the load balancer captures detailed information of all
-- requests and delivers the information to the Amazon S3 bucket that you
-- specify.
--
-- For more information, see
-- <https://docs.aws.amazon.com/elasticloadbalancing/latest/classic/enable-access-logs.html Enable Access Logs>
-- in the /Classic Load Balancers Guide/.
loadBalancerAttributes_accessLog :: Lens.Lens' LoadBalancerAttributes (Prelude.Maybe AccessLog)
loadBalancerAttributes_accessLog = Lens.lens (\LoadBalancerAttributes' {accessLog} -> accessLog) (\s@LoadBalancerAttributes' {} a -> s {accessLog = a} :: LoadBalancerAttributes)

-- | Any additional attributes.
loadBalancerAttributes_additionalAttributes :: Lens.Lens' LoadBalancerAttributes (Prelude.Maybe [AdditionalAttribute])
loadBalancerAttributes_additionalAttributes = Lens.lens (\LoadBalancerAttributes' {additionalAttributes} -> additionalAttributes) (\s@LoadBalancerAttributes' {} a -> s {additionalAttributes = a} :: LoadBalancerAttributes) Prelude.. Lens.mapping Lens.coerced

-- | If enabled, the load balancer allows the connections to remain idle (no
-- data is sent over the connection) for the specified duration.
--
-- By default, Elastic Load Balancing maintains a 60-second idle connection
-- timeout for both front-end and back-end connections of your load
-- balancer. For more information, see
-- <https://docs.aws.amazon.com/elasticloadbalancing/latest/classic/config-idle-timeout.html Configure Idle Connection Timeout>
-- in the /Classic Load Balancers Guide/.
loadBalancerAttributes_connectionSettings :: Lens.Lens' LoadBalancerAttributes (Prelude.Maybe ConnectionSettings)
loadBalancerAttributes_connectionSettings = Lens.lens (\LoadBalancerAttributes' {connectionSettings} -> connectionSettings) (\s@LoadBalancerAttributes' {} a -> s {connectionSettings = a} :: LoadBalancerAttributes)

-- | If enabled, the load balancer allows existing requests to complete
-- before the load balancer shifts traffic away from a deregistered or
-- unhealthy instance.
--
-- For more information, see
-- <https://docs.aws.amazon.com/elasticloadbalancing/latest/classic/config-conn-drain.html Configure Connection Draining>
-- in the /Classic Load Balancers Guide/.
loadBalancerAttributes_connectionDraining :: Lens.Lens' LoadBalancerAttributes (Prelude.Maybe ConnectionDraining)
loadBalancerAttributes_connectionDraining = Lens.lens (\LoadBalancerAttributes' {connectionDraining} -> connectionDraining) (\s@LoadBalancerAttributes' {} a -> s {connectionDraining = a} :: LoadBalancerAttributes)

instance Core.FromXML LoadBalancerAttributes where
  parseXML x =
    LoadBalancerAttributes'
      Prelude.<$> (x Core..@? "CrossZoneLoadBalancing")
      Prelude.<*> (x Core..@? "AccessLog")
      Prelude.<*> ( x Core..@? "AdditionalAttributes"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "member")
                  )
      Prelude.<*> (x Core..@? "ConnectionSettings")
      Prelude.<*> (x Core..@? "ConnectionDraining")

instance Prelude.Hashable LoadBalancerAttributes where
  hashWithSalt salt' LoadBalancerAttributes' {..} =
    salt' `Prelude.hashWithSalt` connectionDraining
      `Prelude.hashWithSalt` connectionSettings
      `Prelude.hashWithSalt` additionalAttributes
      `Prelude.hashWithSalt` accessLog
      `Prelude.hashWithSalt` crossZoneLoadBalancing

instance Prelude.NFData LoadBalancerAttributes where
  rnf LoadBalancerAttributes' {..} =
    Prelude.rnf crossZoneLoadBalancing
      `Prelude.seq` Prelude.rnf connectionDraining
      `Prelude.seq` Prelude.rnf connectionSettings
      `Prelude.seq` Prelude.rnf additionalAttributes
      `Prelude.seq` Prelude.rnf accessLog

instance Core.ToQuery LoadBalancerAttributes where
  toQuery LoadBalancerAttributes' {..} =
    Prelude.mconcat
      [ "CrossZoneLoadBalancing"
          Core.=: crossZoneLoadBalancing,
        "AccessLog" Core.=: accessLog,
        "AdditionalAttributes"
          Core.=: Core.toQuery
            ( Core.toQueryList "member"
                Prelude.<$> additionalAttributes
            ),
        "ConnectionSettings" Core.=: connectionSettings,
        "ConnectionDraining" Core.=: connectionDraining
      ]
