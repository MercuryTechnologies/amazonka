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
-- Module      : Amazonka.ELBV2.Types.Listener
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ELBV2.Types.Listener where

import qualified Amazonka.Core as Core
import Amazonka.ELBV2.Types.Action
import Amazonka.ELBV2.Types.Certificate
import Amazonka.ELBV2.Types.ProtocolEnum
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information about a listener.
--
-- /See:/ 'newListener' smart constructor.
data Listener = Listener'
  { -- | [HTTPS or TLS listener] The security policy that defines which protocols
    -- and ciphers are supported.
    sslPolicy :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the listener.
    listenerArn :: Prelude.Maybe Prelude.Text,
    -- | The protocol for connections from clients to the load balancer.
    protocol :: Prelude.Maybe ProtocolEnum,
    -- | The default actions for the listener.
    defaultActions :: Prelude.Maybe [Action],
    -- | [HTTPS or TLS listener] The default certificate for the listener.
    certificates :: Prelude.Maybe [Certificate],
    -- | The Amazon Resource Name (ARN) of the load balancer.
    loadBalancerArn :: Prelude.Maybe Prelude.Text,
    -- | [TLS listener] The name of the Application-Layer Protocol Negotiation
    -- (ALPN) policy.
    alpnPolicy :: Prelude.Maybe [Prelude.Text],
    -- | The port on which the load balancer is listening.
    port :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Listener' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sslPolicy', 'listener_sslPolicy' - [HTTPS or TLS listener] The security policy that defines which protocols
-- and ciphers are supported.
--
-- 'listenerArn', 'listener_listenerArn' - The Amazon Resource Name (ARN) of the listener.
--
-- 'protocol', 'listener_protocol' - The protocol for connections from clients to the load balancer.
--
-- 'defaultActions', 'listener_defaultActions' - The default actions for the listener.
--
-- 'certificates', 'listener_certificates' - [HTTPS or TLS listener] The default certificate for the listener.
--
-- 'loadBalancerArn', 'listener_loadBalancerArn' - The Amazon Resource Name (ARN) of the load balancer.
--
-- 'alpnPolicy', 'listener_alpnPolicy' - [TLS listener] The name of the Application-Layer Protocol Negotiation
-- (ALPN) policy.
--
-- 'port', 'listener_port' - The port on which the load balancer is listening.
newListener ::
  Listener
newListener =
  Listener'
    { sslPolicy = Prelude.Nothing,
      listenerArn = Prelude.Nothing,
      protocol = Prelude.Nothing,
      defaultActions = Prelude.Nothing,
      certificates = Prelude.Nothing,
      loadBalancerArn = Prelude.Nothing,
      alpnPolicy = Prelude.Nothing,
      port = Prelude.Nothing
    }

-- | [HTTPS or TLS listener] The security policy that defines which protocols
-- and ciphers are supported.
listener_sslPolicy :: Lens.Lens' Listener (Prelude.Maybe Prelude.Text)
listener_sslPolicy = Lens.lens (\Listener' {sslPolicy} -> sslPolicy) (\s@Listener' {} a -> s {sslPolicy = a} :: Listener)

-- | The Amazon Resource Name (ARN) of the listener.
listener_listenerArn :: Lens.Lens' Listener (Prelude.Maybe Prelude.Text)
listener_listenerArn = Lens.lens (\Listener' {listenerArn} -> listenerArn) (\s@Listener' {} a -> s {listenerArn = a} :: Listener)

-- | The protocol for connections from clients to the load balancer.
listener_protocol :: Lens.Lens' Listener (Prelude.Maybe ProtocolEnum)
listener_protocol = Lens.lens (\Listener' {protocol} -> protocol) (\s@Listener' {} a -> s {protocol = a} :: Listener)

-- | The default actions for the listener.
listener_defaultActions :: Lens.Lens' Listener (Prelude.Maybe [Action])
listener_defaultActions = Lens.lens (\Listener' {defaultActions} -> defaultActions) (\s@Listener' {} a -> s {defaultActions = a} :: Listener) Prelude.. Lens.mapping Lens.coerced

-- | [HTTPS or TLS listener] The default certificate for the listener.
listener_certificates :: Lens.Lens' Listener (Prelude.Maybe [Certificate])
listener_certificates = Lens.lens (\Listener' {certificates} -> certificates) (\s@Listener' {} a -> s {certificates = a} :: Listener) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the load balancer.
listener_loadBalancerArn :: Lens.Lens' Listener (Prelude.Maybe Prelude.Text)
listener_loadBalancerArn = Lens.lens (\Listener' {loadBalancerArn} -> loadBalancerArn) (\s@Listener' {} a -> s {loadBalancerArn = a} :: Listener)

-- | [TLS listener] The name of the Application-Layer Protocol Negotiation
-- (ALPN) policy.
listener_alpnPolicy :: Lens.Lens' Listener (Prelude.Maybe [Prelude.Text])
listener_alpnPolicy = Lens.lens (\Listener' {alpnPolicy} -> alpnPolicy) (\s@Listener' {} a -> s {alpnPolicy = a} :: Listener) Prelude.. Lens.mapping Lens.coerced

-- | The port on which the load balancer is listening.
listener_port :: Lens.Lens' Listener (Prelude.Maybe Prelude.Natural)
listener_port = Lens.lens (\Listener' {port} -> port) (\s@Listener' {} a -> s {port = a} :: Listener)

instance Core.FromXML Listener where
  parseXML x =
    Listener'
      Prelude.<$> (x Core..@? "SslPolicy")
      Prelude.<*> (x Core..@? "ListenerArn")
      Prelude.<*> (x Core..@? "Protocol")
      Prelude.<*> ( x Core..@? "DefaultActions" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "member")
                  )
      Prelude.<*> ( x Core..@? "Certificates" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "member")
                  )
      Prelude.<*> (x Core..@? "LoadBalancerArn")
      Prelude.<*> ( x Core..@? "AlpnPolicy" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "member")
                  )
      Prelude.<*> (x Core..@? "Port")

instance Prelude.Hashable Listener where
  hashWithSalt salt' Listener' {..} =
    salt' `Prelude.hashWithSalt` port
      `Prelude.hashWithSalt` alpnPolicy
      `Prelude.hashWithSalt` loadBalancerArn
      `Prelude.hashWithSalt` certificates
      `Prelude.hashWithSalt` defaultActions
      `Prelude.hashWithSalt` protocol
      `Prelude.hashWithSalt` listenerArn
      `Prelude.hashWithSalt` sslPolicy

instance Prelude.NFData Listener where
  rnf Listener' {..} =
    Prelude.rnf sslPolicy
      `Prelude.seq` Prelude.rnf port
      `Prelude.seq` Prelude.rnf alpnPolicy
      `Prelude.seq` Prelude.rnf loadBalancerArn
      `Prelude.seq` Prelude.rnf certificates
      `Prelude.seq` Prelude.rnf defaultActions
      `Prelude.seq` Prelude.rnf protocol
      `Prelude.seq` Prelude.rnf listenerArn
