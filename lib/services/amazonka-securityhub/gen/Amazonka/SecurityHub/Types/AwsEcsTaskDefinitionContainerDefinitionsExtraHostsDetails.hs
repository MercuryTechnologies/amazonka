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
-- Module      : Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionContainerDefinitionsExtraHostsDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEcsTaskDefinitionContainerDefinitionsExtraHostsDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | A hostname and IP address mapping to append to the __\/etc\/hosts__ file
-- on the container.
--
-- /See:/ 'newAwsEcsTaskDefinitionContainerDefinitionsExtraHostsDetails' smart constructor.
data AwsEcsTaskDefinitionContainerDefinitionsExtraHostsDetails = AwsEcsTaskDefinitionContainerDefinitionsExtraHostsDetails'
  { -- | The hostname to use in the __\/etc\/hosts__ entry.
    hostname :: Prelude.Maybe Prelude.Text,
    -- | The IP address to use in the __\/etc\/hosts__ entry.
    ipAddress :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsEcsTaskDefinitionContainerDefinitionsExtraHostsDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hostname', 'awsEcsTaskDefinitionContainerDefinitionsExtraHostsDetails_hostname' - The hostname to use in the __\/etc\/hosts__ entry.
--
-- 'ipAddress', 'awsEcsTaskDefinitionContainerDefinitionsExtraHostsDetails_ipAddress' - The IP address to use in the __\/etc\/hosts__ entry.
newAwsEcsTaskDefinitionContainerDefinitionsExtraHostsDetails ::
  AwsEcsTaskDefinitionContainerDefinitionsExtraHostsDetails
newAwsEcsTaskDefinitionContainerDefinitionsExtraHostsDetails =
  AwsEcsTaskDefinitionContainerDefinitionsExtraHostsDetails'
    { hostname =
        Prelude.Nothing,
      ipAddress =
        Prelude.Nothing
    }

-- | The hostname to use in the __\/etc\/hosts__ entry.
awsEcsTaskDefinitionContainerDefinitionsExtraHostsDetails_hostname :: Lens.Lens' AwsEcsTaskDefinitionContainerDefinitionsExtraHostsDetails (Prelude.Maybe Prelude.Text)
awsEcsTaskDefinitionContainerDefinitionsExtraHostsDetails_hostname = Lens.lens (\AwsEcsTaskDefinitionContainerDefinitionsExtraHostsDetails' {hostname} -> hostname) (\s@AwsEcsTaskDefinitionContainerDefinitionsExtraHostsDetails' {} a -> s {hostname = a} :: AwsEcsTaskDefinitionContainerDefinitionsExtraHostsDetails)

-- | The IP address to use in the __\/etc\/hosts__ entry.
awsEcsTaskDefinitionContainerDefinitionsExtraHostsDetails_ipAddress :: Lens.Lens' AwsEcsTaskDefinitionContainerDefinitionsExtraHostsDetails (Prelude.Maybe Prelude.Text)
awsEcsTaskDefinitionContainerDefinitionsExtraHostsDetails_ipAddress = Lens.lens (\AwsEcsTaskDefinitionContainerDefinitionsExtraHostsDetails' {ipAddress} -> ipAddress) (\s@AwsEcsTaskDefinitionContainerDefinitionsExtraHostsDetails' {} a -> s {ipAddress = a} :: AwsEcsTaskDefinitionContainerDefinitionsExtraHostsDetails)

instance
  Core.FromJSON
    AwsEcsTaskDefinitionContainerDefinitionsExtraHostsDetails
  where
  parseJSON =
    Core.withObject
      "AwsEcsTaskDefinitionContainerDefinitionsExtraHostsDetails"
      ( \x ->
          AwsEcsTaskDefinitionContainerDefinitionsExtraHostsDetails'
            Prelude.<$> (x Core..:? "Hostname")
              Prelude.<*> (x Core..:? "IpAddress")
      )

instance
  Prelude.Hashable
    AwsEcsTaskDefinitionContainerDefinitionsExtraHostsDetails
  where
  hashWithSalt
    salt'
    AwsEcsTaskDefinitionContainerDefinitionsExtraHostsDetails' {..} =
      salt' `Prelude.hashWithSalt` ipAddress
        `Prelude.hashWithSalt` hostname

instance
  Prelude.NFData
    AwsEcsTaskDefinitionContainerDefinitionsExtraHostsDetails
  where
  rnf
    AwsEcsTaskDefinitionContainerDefinitionsExtraHostsDetails' {..} =
      Prelude.rnf hostname
        `Prelude.seq` Prelude.rnf ipAddress

instance
  Core.ToJSON
    AwsEcsTaskDefinitionContainerDefinitionsExtraHostsDetails
  where
  toJSON
    AwsEcsTaskDefinitionContainerDefinitionsExtraHostsDetails' {..} =
      Core.object
        ( Prelude.catMaybes
            [ ("Hostname" Core..=) Prelude.<$> hostname,
              ("IpAddress" Core..=) Prelude.<$> ipAddress
            ]
        )
