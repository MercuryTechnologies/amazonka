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
-- Module      : Amazonka.SecurityHub.Types.ActionLocalIpDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.ActionLocalIpDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Provides information about the IP address where the scanned port is
-- located.
--
-- /See:/ 'newActionLocalIpDetails' smart constructor.
data ActionLocalIpDetails = ActionLocalIpDetails'
  { -- | The IP address.
    ipAddressV4 :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ActionLocalIpDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ipAddressV4', 'actionLocalIpDetails_ipAddressV4' - The IP address.
newActionLocalIpDetails ::
  ActionLocalIpDetails
newActionLocalIpDetails =
  ActionLocalIpDetails'
    { ipAddressV4 =
        Prelude.Nothing
    }

-- | The IP address.
actionLocalIpDetails_ipAddressV4 :: Lens.Lens' ActionLocalIpDetails (Prelude.Maybe Prelude.Text)
actionLocalIpDetails_ipAddressV4 = Lens.lens (\ActionLocalIpDetails' {ipAddressV4} -> ipAddressV4) (\s@ActionLocalIpDetails' {} a -> s {ipAddressV4 = a} :: ActionLocalIpDetails)

instance Core.FromJSON ActionLocalIpDetails where
  parseJSON =
    Core.withObject
      "ActionLocalIpDetails"
      ( \x ->
          ActionLocalIpDetails'
            Prelude.<$> (x Core..:? "IpAddressV4")
      )

instance Prelude.Hashable ActionLocalIpDetails where
  hashWithSalt salt' ActionLocalIpDetails' {..} =
    salt' `Prelude.hashWithSalt` ipAddressV4

instance Prelude.NFData ActionLocalIpDetails where
  rnf ActionLocalIpDetails' {..} =
    Prelude.rnf ipAddressV4

instance Core.ToJSON ActionLocalIpDetails where
  toJSON ActionLocalIpDetails' {..} =
    Core.object
      ( Prelude.catMaybes
          [("IpAddressV4" Core..=) Prelude.<$> ipAddressV4]
      )
