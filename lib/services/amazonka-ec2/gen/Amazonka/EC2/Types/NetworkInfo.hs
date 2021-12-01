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
-- Module      : Amazonka.EC2.Types.NetworkInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.NetworkInfo where

import qualified Amazonka.Core as Core
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.EfaInfo
import Amazonka.EC2.Types.EnaSupport
import Amazonka.EC2.Types.NetworkCardInfo
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Describes the networking features of the instance type.
--
-- /See:/ 'newNetworkInfo' smart constructor.
data NetworkInfo = NetworkInfo'
  { -- | Indicates whether Elastic Fabric Adapter (EFA) is supported.
    efaSupported :: Prelude.Maybe Prelude.Bool,
    -- | Indicates whether IPv6 is supported.
    ipv6Supported :: Prelude.Maybe Prelude.Bool,
    -- | Indicates whether Elastic Network Adapter (ENA) is supported.
    enaSupport :: Prelude.Maybe EnaSupport,
    -- | The maximum number of network interfaces for the instance type.
    maximumNetworkInterfaces :: Prelude.Maybe Prelude.Int,
    -- | The maximum number of IPv6 addresses per network interface.
    ipv6AddressesPerInterface :: Prelude.Maybe Prelude.Int,
    -- | The network performance.
    networkPerformance :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of physical network cards that can be allocated to
    -- the instance.
    maximumNetworkCards :: Prelude.Maybe Prelude.Int,
    -- | Describes the network cards for the instance type.
    networkCards :: Prelude.Maybe [NetworkCardInfo],
    -- | Describes the Elastic Fabric Adapters for the instance type.
    efaInfo :: Prelude.Maybe EfaInfo,
    -- | The index of the default network card, starting at 0.
    defaultNetworkCardIndex :: Prelude.Maybe Prelude.Int,
    -- | The maximum number of IPv4 addresses per network interface.
    ipv4AddressesPerInterface :: Prelude.Maybe Prelude.Int,
    -- | Indicates whether the instance type automatically encrypts in-transit
    -- traffic between instances.
    encryptionInTransitSupported :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NetworkInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'efaSupported', 'networkInfo_efaSupported' - Indicates whether Elastic Fabric Adapter (EFA) is supported.
--
-- 'ipv6Supported', 'networkInfo_ipv6Supported' - Indicates whether IPv6 is supported.
--
-- 'enaSupport', 'networkInfo_enaSupport' - Indicates whether Elastic Network Adapter (ENA) is supported.
--
-- 'maximumNetworkInterfaces', 'networkInfo_maximumNetworkInterfaces' - The maximum number of network interfaces for the instance type.
--
-- 'ipv6AddressesPerInterface', 'networkInfo_ipv6AddressesPerInterface' - The maximum number of IPv6 addresses per network interface.
--
-- 'networkPerformance', 'networkInfo_networkPerformance' - The network performance.
--
-- 'maximumNetworkCards', 'networkInfo_maximumNetworkCards' - The maximum number of physical network cards that can be allocated to
-- the instance.
--
-- 'networkCards', 'networkInfo_networkCards' - Describes the network cards for the instance type.
--
-- 'efaInfo', 'networkInfo_efaInfo' - Describes the Elastic Fabric Adapters for the instance type.
--
-- 'defaultNetworkCardIndex', 'networkInfo_defaultNetworkCardIndex' - The index of the default network card, starting at 0.
--
-- 'ipv4AddressesPerInterface', 'networkInfo_ipv4AddressesPerInterface' - The maximum number of IPv4 addresses per network interface.
--
-- 'encryptionInTransitSupported', 'networkInfo_encryptionInTransitSupported' - Indicates whether the instance type automatically encrypts in-transit
-- traffic between instances.
newNetworkInfo ::
  NetworkInfo
newNetworkInfo =
  NetworkInfo'
    { efaSupported = Prelude.Nothing,
      ipv6Supported = Prelude.Nothing,
      enaSupport = Prelude.Nothing,
      maximumNetworkInterfaces = Prelude.Nothing,
      ipv6AddressesPerInterface = Prelude.Nothing,
      networkPerformance = Prelude.Nothing,
      maximumNetworkCards = Prelude.Nothing,
      networkCards = Prelude.Nothing,
      efaInfo = Prelude.Nothing,
      defaultNetworkCardIndex = Prelude.Nothing,
      ipv4AddressesPerInterface = Prelude.Nothing,
      encryptionInTransitSupported = Prelude.Nothing
    }

-- | Indicates whether Elastic Fabric Adapter (EFA) is supported.
networkInfo_efaSupported :: Lens.Lens' NetworkInfo (Prelude.Maybe Prelude.Bool)
networkInfo_efaSupported = Lens.lens (\NetworkInfo' {efaSupported} -> efaSupported) (\s@NetworkInfo' {} a -> s {efaSupported = a} :: NetworkInfo)

-- | Indicates whether IPv6 is supported.
networkInfo_ipv6Supported :: Lens.Lens' NetworkInfo (Prelude.Maybe Prelude.Bool)
networkInfo_ipv6Supported = Lens.lens (\NetworkInfo' {ipv6Supported} -> ipv6Supported) (\s@NetworkInfo' {} a -> s {ipv6Supported = a} :: NetworkInfo)

-- | Indicates whether Elastic Network Adapter (ENA) is supported.
networkInfo_enaSupport :: Lens.Lens' NetworkInfo (Prelude.Maybe EnaSupport)
networkInfo_enaSupport = Lens.lens (\NetworkInfo' {enaSupport} -> enaSupport) (\s@NetworkInfo' {} a -> s {enaSupport = a} :: NetworkInfo)

-- | The maximum number of network interfaces for the instance type.
networkInfo_maximumNetworkInterfaces :: Lens.Lens' NetworkInfo (Prelude.Maybe Prelude.Int)
networkInfo_maximumNetworkInterfaces = Lens.lens (\NetworkInfo' {maximumNetworkInterfaces} -> maximumNetworkInterfaces) (\s@NetworkInfo' {} a -> s {maximumNetworkInterfaces = a} :: NetworkInfo)

-- | The maximum number of IPv6 addresses per network interface.
networkInfo_ipv6AddressesPerInterface :: Lens.Lens' NetworkInfo (Prelude.Maybe Prelude.Int)
networkInfo_ipv6AddressesPerInterface = Lens.lens (\NetworkInfo' {ipv6AddressesPerInterface} -> ipv6AddressesPerInterface) (\s@NetworkInfo' {} a -> s {ipv6AddressesPerInterface = a} :: NetworkInfo)

-- | The network performance.
networkInfo_networkPerformance :: Lens.Lens' NetworkInfo (Prelude.Maybe Prelude.Text)
networkInfo_networkPerformance = Lens.lens (\NetworkInfo' {networkPerformance} -> networkPerformance) (\s@NetworkInfo' {} a -> s {networkPerformance = a} :: NetworkInfo)

-- | The maximum number of physical network cards that can be allocated to
-- the instance.
networkInfo_maximumNetworkCards :: Lens.Lens' NetworkInfo (Prelude.Maybe Prelude.Int)
networkInfo_maximumNetworkCards = Lens.lens (\NetworkInfo' {maximumNetworkCards} -> maximumNetworkCards) (\s@NetworkInfo' {} a -> s {maximumNetworkCards = a} :: NetworkInfo)

-- | Describes the network cards for the instance type.
networkInfo_networkCards :: Lens.Lens' NetworkInfo (Prelude.Maybe [NetworkCardInfo])
networkInfo_networkCards = Lens.lens (\NetworkInfo' {networkCards} -> networkCards) (\s@NetworkInfo' {} a -> s {networkCards = a} :: NetworkInfo) Prelude.. Lens.mapping Lens.coerced

-- | Describes the Elastic Fabric Adapters for the instance type.
networkInfo_efaInfo :: Lens.Lens' NetworkInfo (Prelude.Maybe EfaInfo)
networkInfo_efaInfo = Lens.lens (\NetworkInfo' {efaInfo} -> efaInfo) (\s@NetworkInfo' {} a -> s {efaInfo = a} :: NetworkInfo)

-- | The index of the default network card, starting at 0.
networkInfo_defaultNetworkCardIndex :: Lens.Lens' NetworkInfo (Prelude.Maybe Prelude.Int)
networkInfo_defaultNetworkCardIndex = Lens.lens (\NetworkInfo' {defaultNetworkCardIndex} -> defaultNetworkCardIndex) (\s@NetworkInfo' {} a -> s {defaultNetworkCardIndex = a} :: NetworkInfo)

-- | The maximum number of IPv4 addresses per network interface.
networkInfo_ipv4AddressesPerInterface :: Lens.Lens' NetworkInfo (Prelude.Maybe Prelude.Int)
networkInfo_ipv4AddressesPerInterface = Lens.lens (\NetworkInfo' {ipv4AddressesPerInterface} -> ipv4AddressesPerInterface) (\s@NetworkInfo' {} a -> s {ipv4AddressesPerInterface = a} :: NetworkInfo)

-- | Indicates whether the instance type automatically encrypts in-transit
-- traffic between instances.
networkInfo_encryptionInTransitSupported :: Lens.Lens' NetworkInfo (Prelude.Maybe Prelude.Bool)
networkInfo_encryptionInTransitSupported = Lens.lens (\NetworkInfo' {encryptionInTransitSupported} -> encryptionInTransitSupported) (\s@NetworkInfo' {} a -> s {encryptionInTransitSupported = a} :: NetworkInfo)

instance Core.FromXML NetworkInfo where
  parseXML x =
    NetworkInfo'
      Prelude.<$> (x Core..@? "efaSupported")
      Prelude.<*> (x Core..@? "ipv6Supported")
      Prelude.<*> (x Core..@? "enaSupport")
      Prelude.<*> (x Core..@? "maximumNetworkInterfaces")
      Prelude.<*> (x Core..@? "ipv6AddressesPerInterface")
      Prelude.<*> (x Core..@? "networkPerformance")
      Prelude.<*> (x Core..@? "maximumNetworkCards")
      Prelude.<*> ( x Core..@? "networkCards" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "item")
                  )
      Prelude.<*> (x Core..@? "efaInfo")
      Prelude.<*> (x Core..@? "defaultNetworkCardIndex")
      Prelude.<*> (x Core..@? "ipv4AddressesPerInterface")
      Prelude.<*> (x Core..@? "encryptionInTransitSupported")

instance Prelude.Hashable NetworkInfo where
  hashWithSalt salt' NetworkInfo' {..} =
    salt'
      `Prelude.hashWithSalt` encryptionInTransitSupported
      `Prelude.hashWithSalt` ipv4AddressesPerInterface
      `Prelude.hashWithSalt` defaultNetworkCardIndex
      `Prelude.hashWithSalt` efaInfo
      `Prelude.hashWithSalt` networkCards
      `Prelude.hashWithSalt` maximumNetworkCards
      `Prelude.hashWithSalt` networkPerformance
      `Prelude.hashWithSalt` ipv6AddressesPerInterface
      `Prelude.hashWithSalt` maximumNetworkInterfaces
      `Prelude.hashWithSalt` enaSupport
      `Prelude.hashWithSalt` ipv6Supported
      `Prelude.hashWithSalt` efaSupported

instance Prelude.NFData NetworkInfo where
  rnf NetworkInfo' {..} =
    Prelude.rnf efaSupported
      `Prelude.seq` Prelude.rnf encryptionInTransitSupported
      `Prelude.seq` Prelude.rnf ipv4AddressesPerInterface
      `Prelude.seq` Prelude.rnf defaultNetworkCardIndex
      `Prelude.seq` Prelude.rnf efaInfo
      `Prelude.seq` Prelude.rnf networkCards
      `Prelude.seq` Prelude.rnf maximumNetworkCards
      `Prelude.seq` Prelude.rnf networkPerformance
      `Prelude.seq` Prelude.rnf ipv6AddressesPerInterface
      `Prelude.seq` Prelude.rnf maximumNetworkInterfaces
      `Prelude.seq` Prelude.rnf enaSupport
      `Prelude.seq` Prelude.rnf ipv6Supported
