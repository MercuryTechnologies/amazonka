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
-- Module      : Amazonka.MQ.Types.BrokerInstance
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MQ.Types.BrokerInstance where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Returns information about all brokers.
--
-- /See:/ 'newBrokerInstance' smart constructor.
data BrokerInstance = BrokerInstance'
  { -- | The IP address of the Elastic Network Interface (ENI) attached to the
    -- broker. Does not apply to RabbitMQ brokers.
    ipAddress :: Prelude.Maybe Prelude.Text,
    -- | The brokers web console URL.
    consoleURL :: Prelude.Maybe Prelude.Text,
    -- | The broker\'s wire-level protocol endpoints.
    endpoints :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BrokerInstance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ipAddress', 'brokerInstance_ipAddress' - The IP address of the Elastic Network Interface (ENI) attached to the
-- broker. Does not apply to RabbitMQ brokers.
--
-- 'consoleURL', 'brokerInstance_consoleURL' - The brokers web console URL.
--
-- 'endpoints', 'brokerInstance_endpoints' - The broker\'s wire-level protocol endpoints.
newBrokerInstance ::
  BrokerInstance
newBrokerInstance =
  BrokerInstance'
    { ipAddress = Prelude.Nothing,
      consoleURL = Prelude.Nothing,
      endpoints = Prelude.Nothing
    }

-- | The IP address of the Elastic Network Interface (ENI) attached to the
-- broker. Does not apply to RabbitMQ brokers.
brokerInstance_ipAddress :: Lens.Lens' BrokerInstance (Prelude.Maybe Prelude.Text)
brokerInstance_ipAddress = Lens.lens (\BrokerInstance' {ipAddress} -> ipAddress) (\s@BrokerInstance' {} a -> s {ipAddress = a} :: BrokerInstance)

-- | The brokers web console URL.
brokerInstance_consoleURL :: Lens.Lens' BrokerInstance (Prelude.Maybe Prelude.Text)
brokerInstance_consoleURL = Lens.lens (\BrokerInstance' {consoleURL} -> consoleURL) (\s@BrokerInstance' {} a -> s {consoleURL = a} :: BrokerInstance)

-- | The broker\'s wire-level protocol endpoints.
brokerInstance_endpoints :: Lens.Lens' BrokerInstance (Prelude.Maybe [Prelude.Text])
brokerInstance_endpoints = Lens.lens (\BrokerInstance' {endpoints} -> endpoints) (\s@BrokerInstance' {} a -> s {endpoints = a} :: BrokerInstance) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON BrokerInstance where
  parseJSON =
    Core.withObject
      "BrokerInstance"
      ( \x ->
          BrokerInstance'
            Prelude.<$> (x Core..:? "ipAddress")
            Prelude.<*> (x Core..:? "consoleURL")
            Prelude.<*> (x Core..:? "endpoints" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable BrokerInstance where
  hashWithSalt salt' BrokerInstance' {..} =
    salt' `Prelude.hashWithSalt` endpoints
      `Prelude.hashWithSalt` consoleURL
      `Prelude.hashWithSalt` ipAddress

instance Prelude.NFData BrokerInstance where
  rnf BrokerInstance' {..} =
    Prelude.rnf ipAddress
      `Prelude.seq` Prelude.rnf endpoints
      `Prelude.seq` Prelude.rnf consoleURL
