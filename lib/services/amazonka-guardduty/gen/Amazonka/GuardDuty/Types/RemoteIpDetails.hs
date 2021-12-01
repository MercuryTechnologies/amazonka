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
-- Module      : Amazonka.GuardDuty.Types.RemoteIpDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GuardDuty.Types.RemoteIpDetails where

import qualified Amazonka.Core as Core
import Amazonka.GuardDuty.Types.City
import Amazonka.GuardDuty.Types.Country
import Amazonka.GuardDuty.Types.GeoLocation
import Amazonka.GuardDuty.Types.Organization
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the remote IP address of the connection.
--
-- /See:/ 'newRemoteIpDetails' smart constructor.
data RemoteIpDetails = RemoteIpDetails'
  { -- | The country code of the remote IP address.
    country :: Prelude.Maybe Country,
    -- | The city information of the remote IP address.
    city :: Prelude.Maybe City,
    -- | The IPv4 remote address of the connection.
    ipAddressV4 :: Prelude.Maybe Prelude.Text,
    -- | The location information of the remote IP address.
    geoLocation :: Prelude.Maybe GeoLocation,
    -- | The ISP organization information of the remote IP address.
    organization :: Prelude.Maybe Organization
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RemoteIpDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'country', 'remoteIpDetails_country' - The country code of the remote IP address.
--
-- 'city', 'remoteIpDetails_city' - The city information of the remote IP address.
--
-- 'ipAddressV4', 'remoteIpDetails_ipAddressV4' - The IPv4 remote address of the connection.
--
-- 'geoLocation', 'remoteIpDetails_geoLocation' - The location information of the remote IP address.
--
-- 'organization', 'remoteIpDetails_organization' - The ISP organization information of the remote IP address.
newRemoteIpDetails ::
  RemoteIpDetails
newRemoteIpDetails =
  RemoteIpDetails'
    { country = Prelude.Nothing,
      city = Prelude.Nothing,
      ipAddressV4 = Prelude.Nothing,
      geoLocation = Prelude.Nothing,
      organization = Prelude.Nothing
    }

-- | The country code of the remote IP address.
remoteIpDetails_country :: Lens.Lens' RemoteIpDetails (Prelude.Maybe Country)
remoteIpDetails_country = Lens.lens (\RemoteIpDetails' {country} -> country) (\s@RemoteIpDetails' {} a -> s {country = a} :: RemoteIpDetails)

-- | The city information of the remote IP address.
remoteIpDetails_city :: Lens.Lens' RemoteIpDetails (Prelude.Maybe City)
remoteIpDetails_city = Lens.lens (\RemoteIpDetails' {city} -> city) (\s@RemoteIpDetails' {} a -> s {city = a} :: RemoteIpDetails)

-- | The IPv4 remote address of the connection.
remoteIpDetails_ipAddressV4 :: Lens.Lens' RemoteIpDetails (Prelude.Maybe Prelude.Text)
remoteIpDetails_ipAddressV4 = Lens.lens (\RemoteIpDetails' {ipAddressV4} -> ipAddressV4) (\s@RemoteIpDetails' {} a -> s {ipAddressV4 = a} :: RemoteIpDetails)

-- | The location information of the remote IP address.
remoteIpDetails_geoLocation :: Lens.Lens' RemoteIpDetails (Prelude.Maybe GeoLocation)
remoteIpDetails_geoLocation = Lens.lens (\RemoteIpDetails' {geoLocation} -> geoLocation) (\s@RemoteIpDetails' {} a -> s {geoLocation = a} :: RemoteIpDetails)

-- | The ISP organization information of the remote IP address.
remoteIpDetails_organization :: Lens.Lens' RemoteIpDetails (Prelude.Maybe Organization)
remoteIpDetails_organization = Lens.lens (\RemoteIpDetails' {organization} -> organization) (\s@RemoteIpDetails' {} a -> s {organization = a} :: RemoteIpDetails)

instance Core.FromJSON RemoteIpDetails where
  parseJSON =
    Core.withObject
      "RemoteIpDetails"
      ( \x ->
          RemoteIpDetails'
            Prelude.<$> (x Core..:? "country")
            Prelude.<*> (x Core..:? "city")
            Prelude.<*> (x Core..:? "ipAddressV4")
            Prelude.<*> (x Core..:? "geoLocation")
            Prelude.<*> (x Core..:? "organization")
      )

instance Prelude.Hashable RemoteIpDetails where
  hashWithSalt salt' RemoteIpDetails' {..} =
    salt' `Prelude.hashWithSalt` organization
      `Prelude.hashWithSalt` geoLocation
      `Prelude.hashWithSalt` ipAddressV4
      `Prelude.hashWithSalt` city
      `Prelude.hashWithSalt` country

instance Prelude.NFData RemoteIpDetails where
  rnf RemoteIpDetails' {..} =
    Prelude.rnf country
      `Prelude.seq` Prelude.rnf organization
      `Prelude.seq` Prelude.rnf geoLocation
      `Prelude.seq` Prelude.rnf ipAddressV4
      `Prelude.seq` Prelude.rnf city
