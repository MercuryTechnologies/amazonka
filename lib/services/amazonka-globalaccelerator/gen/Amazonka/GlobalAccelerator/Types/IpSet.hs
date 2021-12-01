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
-- Module      : Amazonka.GlobalAccelerator.Types.IpSet
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GlobalAccelerator.Types.IpSet where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | A complex type for the set of IP addresses for an accelerator.
--
-- /See:/ 'newIpSet' smart constructor.
data IpSet = IpSet'
  { -- | The types of IP addresses included in this IP set.
    ipFamily :: Prelude.Maybe Prelude.Text,
    -- | The array of IP addresses in the IP address set. An IP address set can
    -- have a maximum of two IP addresses.
    ipAddresses :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'IpSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ipFamily', 'ipSet_ipFamily' - The types of IP addresses included in this IP set.
--
-- 'ipAddresses', 'ipSet_ipAddresses' - The array of IP addresses in the IP address set. An IP address set can
-- have a maximum of two IP addresses.
newIpSet ::
  IpSet
newIpSet =
  IpSet'
    { ipFamily = Prelude.Nothing,
      ipAddresses = Prelude.Nothing
    }

-- | The types of IP addresses included in this IP set.
ipSet_ipFamily :: Lens.Lens' IpSet (Prelude.Maybe Prelude.Text)
ipSet_ipFamily = Lens.lens (\IpSet' {ipFamily} -> ipFamily) (\s@IpSet' {} a -> s {ipFamily = a} :: IpSet)

-- | The array of IP addresses in the IP address set. An IP address set can
-- have a maximum of two IP addresses.
ipSet_ipAddresses :: Lens.Lens' IpSet (Prelude.Maybe [Prelude.Text])
ipSet_ipAddresses = Lens.lens (\IpSet' {ipAddresses} -> ipAddresses) (\s@IpSet' {} a -> s {ipAddresses = a} :: IpSet) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON IpSet where
  parseJSON =
    Core.withObject
      "IpSet"
      ( \x ->
          IpSet'
            Prelude.<$> (x Core..:? "IpFamily")
            Prelude.<*> (x Core..:? "IpAddresses" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable IpSet where
  hashWithSalt salt' IpSet' {..} =
    salt' `Prelude.hashWithSalt` ipAddresses
      `Prelude.hashWithSalt` ipFamily

instance Prelude.NFData IpSet where
  rnf IpSet' {..} =
    Prelude.rnf ipFamily
      `Prelude.seq` Prelude.rnf ipAddresses
