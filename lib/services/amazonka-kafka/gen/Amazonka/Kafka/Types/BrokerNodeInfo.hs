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
-- Module      : Amazonka.Kafka.Types.BrokerNodeInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kafka.Types.BrokerNodeInfo where

import qualified Amazonka.Core as Core
import Amazonka.Kafka.Types.BrokerSoftwareInfo
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | BrokerNodeInfo
--
-- /See:/ 'newBrokerNodeInfo' smart constructor.
data BrokerNodeInfo = BrokerNodeInfo'
  { -- | Information about the version of software currently deployed on the
    -- Kafka brokers in the cluster.
    currentBrokerSoftwareInfo :: Prelude.Maybe BrokerSoftwareInfo,
    -- | The client subnet to which this broker node belongs.
    clientSubnet :: Prelude.Maybe Prelude.Text,
    -- | The attached elastic network interface of the broker.
    attachedENIId :: Prelude.Maybe Prelude.Text,
    -- | Endpoints for accessing the broker.
    endpoints :: Prelude.Maybe [Prelude.Text],
    -- | The virtual private cloud (VPC) of the client.
    clientVpcIpAddress :: Prelude.Maybe Prelude.Text,
    -- | The ID of the broker.
    brokerId :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BrokerNodeInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'currentBrokerSoftwareInfo', 'brokerNodeInfo_currentBrokerSoftwareInfo' - Information about the version of software currently deployed on the
-- Kafka brokers in the cluster.
--
-- 'clientSubnet', 'brokerNodeInfo_clientSubnet' - The client subnet to which this broker node belongs.
--
-- 'attachedENIId', 'brokerNodeInfo_attachedENIId' - The attached elastic network interface of the broker.
--
-- 'endpoints', 'brokerNodeInfo_endpoints' - Endpoints for accessing the broker.
--
-- 'clientVpcIpAddress', 'brokerNodeInfo_clientVpcIpAddress' - The virtual private cloud (VPC) of the client.
--
-- 'brokerId', 'brokerNodeInfo_brokerId' - The ID of the broker.
newBrokerNodeInfo ::
  BrokerNodeInfo
newBrokerNodeInfo =
  BrokerNodeInfo'
    { currentBrokerSoftwareInfo =
        Prelude.Nothing,
      clientSubnet = Prelude.Nothing,
      attachedENIId = Prelude.Nothing,
      endpoints = Prelude.Nothing,
      clientVpcIpAddress = Prelude.Nothing,
      brokerId = Prelude.Nothing
    }

-- | Information about the version of software currently deployed on the
-- Kafka brokers in the cluster.
brokerNodeInfo_currentBrokerSoftwareInfo :: Lens.Lens' BrokerNodeInfo (Prelude.Maybe BrokerSoftwareInfo)
brokerNodeInfo_currentBrokerSoftwareInfo = Lens.lens (\BrokerNodeInfo' {currentBrokerSoftwareInfo} -> currentBrokerSoftwareInfo) (\s@BrokerNodeInfo' {} a -> s {currentBrokerSoftwareInfo = a} :: BrokerNodeInfo)

-- | The client subnet to which this broker node belongs.
brokerNodeInfo_clientSubnet :: Lens.Lens' BrokerNodeInfo (Prelude.Maybe Prelude.Text)
brokerNodeInfo_clientSubnet = Lens.lens (\BrokerNodeInfo' {clientSubnet} -> clientSubnet) (\s@BrokerNodeInfo' {} a -> s {clientSubnet = a} :: BrokerNodeInfo)

-- | The attached elastic network interface of the broker.
brokerNodeInfo_attachedENIId :: Lens.Lens' BrokerNodeInfo (Prelude.Maybe Prelude.Text)
brokerNodeInfo_attachedENIId = Lens.lens (\BrokerNodeInfo' {attachedENIId} -> attachedENIId) (\s@BrokerNodeInfo' {} a -> s {attachedENIId = a} :: BrokerNodeInfo)

-- | Endpoints for accessing the broker.
brokerNodeInfo_endpoints :: Lens.Lens' BrokerNodeInfo (Prelude.Maybe [Prelude.Text])
brokerNodeInfo_endpoints = Lens.lens (\BrokerNodeInfo' {endpoints} -> endpoints) (\s@BrokerNodeInfo' {} a -> s {endpoints = a} :: BrokerNodeInfo) Prelude.. Lens.mapping Lens.coerced

-- | The virtual private cloud (VPC) of the client.
brokerNodeInfo_clientVpcIpAddress :: Lens.Lens' BrokerNodeInfo (Prelude.Maybe Prelude.Text)
brokerNodeInfo_clientVpcIpAddress = Lens.lens (\BrokerNodeInfo' {clientVpcIpAddress} -> clientVpcIpAddress) (\s@BrokerNodeInfo' {} a -> s {clientVpcIpAddress = a} :: BrokerNodeInfo)

-- | The ID of the broker.
brokerNodeInfo_brokerId :: Lens.Lens' BrokerNodeInfo (Prelude.Maybe Prelude.Double)
brokerNodeInfo_brokerId = Lens.lens (\BrokerNodeInfo' {brokerId} -> brokerId) (\s@BrokerNodeInfo' {} a -> s {brokerId = a} :: BrokerNodeInfo)

instance Core.FromJSON BrokerNodeInfo where
  parseJSON =
    Core.withObject
      "BrokerNodeInfo"
      ( \x ->
          BrokerNodeInfo'
            Prelude.<$> (x Core..:? "currentBrokerSoftwareInfo")
            Prelude.<*> (x Core..:? "clientSubnet")
            Prelude.<*> (x Core..:? "attachedENIId")
            Prelude.<*> (x Core..:? "endpoints" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "clientVpcIpAddress")
            Prelude.<*> (x Core..:? "brokerId")
      )

instance Prelude.Hashable BrokerNodeInfo where
  hashWithSalt salt' BrokerNodeInfo' {..} =
    salt' `Prelude.hashWithSalt` brokerId
      `Prelude.hashWithSalt` clientVpcIpAddress
      `Prelude.hashWithSalt` endpoints
      `Prelude.hashWithSalt` attachedENIId
      `Prelude.hashWithSalt` clientSubnet
      `Prelude.hashWithSalt` currentBrokerSoftwareInfo

instance Prelude.NFData BrokerNodeInfo where
  rnf BrokerNodeInfo' {..} =
    Prelude.rnf currentBrokerSoftwareInfo
      `Prelude.seq` Prelude.rnf brokerId
      `Prelude.seq` Prelude.rnf clientVpcIpAddress
      `Prelude.seq` Prelude.rnf endpoints
      `Prelude.seq` Prelude.rnf attachedENIId
      `Prelude.seq` Prelude.rnf clientSubnet
