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
-- Module      : Amazonka.AppFlow.Types.CustomerProfilesDestinationProperties
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AppFlow.Types.CustomerProfilesDestinationProperties where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The properties that are applied when Amazon Connect Customer Profiles is
-- used as a destination.
--
-- /See:/ 'newCustomerProfilesDestinationProperties' smart constructor.
data CustomerProfilesDestinationProperties = CustomerProfilesDestinationProperties'
  { -- | The object specified in the Amazon Connect Customer Profiles flow
    -- destination.
    objectTypeName :: Prelude.Maybe Prelude.Text,
    -- | The unique name of the Amazon Connect Customer Profiles domain.
    domainName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CustomerProfilesDestinationProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'objectTypeName', 'customerProfilesDestinationProperties_objectTypeName' - The object specified in the Amazon Connect Customer Profiles flow
-- destination.
--
-- 'domainName', 'customerProfilesDestinationProperties_domainName' - The unique name of the Amazon Connect Customer Profiles domain.
newCustomerProfilesDestinationProperties ::
  -- | 'domainName'
  Prelude.Text ->
  CustomerProfilesDestinationProperties
newCustomerProfilesDestinationProperties pDomainName_ =
  CustomerProfilesDestinationProperties'
    { objectTypeName =
        Prelude.Nothing,
      domainName = pDomainName_
    }

-- | The object specified in the Amazon Connect Customer Profiles flow
-- destination.
customerProfilesDestinationProperties_objectTypeName :: Lens.Lens' CustomerProfilesDestinationProperties (Prelude.Maybe Prelude.Text)
customerProfilesDestinationProperties_objectTypeName = Lens.lens (\CustomerProfilesDestinationProperties' {objectTypeName} -> objectTypeName) (\s@CustomerProfilesDestinationProperties' {} a -> s {objectTypeName = a} :: CustomerProfilesDestinationProperties)

-- | The unique name of the Amazon Connect Customer Profiles domain.
customerProfilesDestinationProperties_domainName :: Lens.Lens' CustomerProfilesDestinationProperties Prelude.Text
customerProfilesDestinationProperties_domainName = Lens.lens (\CustomerProfilesDestinationProperties' {domainName} -> domainName) (\s@CustomerProfilesDestinationProperties' {} a -> s {domainName = a} :: CustomerProfilesDestinationProperties)

instance
  Core.FromJSON
    CustomerProfilesDestinationProperties
  where
  parseJSON =
    Core.withObject
      "CustomerProfilesDestinationProperties"
      ( \x ->
          CustomerProfilesDestinationProperties'
            Prelude.<$> (x Core..:? "objectTypeName")
            Prelude.<*> (x Core..: "domainName")
      )

instance
  Prelude.Hashable
    CustomerProfilesDestinationProperties
  where
  hashWithSalt
    salt'
    CustomerProfilesDestinationProperties' {..} =
      salt' `Prelude.hashWithSalt` domainName
        `Prelude.hashWithSalt` objectTypeName

instance
  Prelude.NFData
    CustomerProfilesDestinationProperties
  where
  rnf CustomerProfilesDestinationProperties' {..} =
    Prelude.rnf objectTypeName
      `Prelude.seq` Prelude.rnf domainName

instance
  Core.ToJSON
    CustomerProfilesDestinationProperties
  where
  toJSON CustomerProfilesDestinationProperties' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("objectTypeName" Core..=)
              Prelude.<$> objectTypeName,
            Prelude.Just ("domainName" Core..= domainName)
          ]
      )
