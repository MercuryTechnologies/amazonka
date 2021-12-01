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
-- Module      : Amazonka.Grafana.Types.SamlConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Grafana.Types.SamlConfiguration where

import qualified Amazonka.Core as Core
import Amazonka.Grafana.Types.AssertionAttributes
import Amazonka.Grafana.Types.IdpMetadata
import Amazonka.Grafana.Types.RoleValues
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | A structure containing information about how this workspace works with
-- SAML.
--
-- /See:/ 'newSamlConfiguration' smart constructor.
data SamlConfiguration = SamlConfiguration'
  { -- | How long a sign-on session by a SAML user is valid, before the user has
    -- to sign on again.
    loginValidityDuration :: Prelude.Maybe Prelude.Int,
    -- | A structure that defines which attributes in the SAML assertion are to
    -- be used to define information about the users authenticated by that IdP
    -- to use the workspace.
    assertionAttributes :: Prelude.Maybe AssertionAttributes,
    -- | Lists which organizations defined in the SAML assertion are allowed to
    -- use the Amazon Managed Grafana workspace. If this is empty, all
    -- organizations in the assertion attribute have access.
    allowedOrganizations :: Prelude.Maybe [Prelude.Text],
    -- | A structure containing arrays that map group names in the SAML assertion
    -- to the Grafana @Admin@ and @Editor@ roles in the workspace.
    roleValues :: Prelude.Maybe RoleValues,
    -- | A structure containing the identity provider (IdP) metadata used to
    -- integrate the identity provider with this workspace.
    idpMetadata :: IdpMetadata
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SamlConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'loginValidityDuration', 'samlConfiguration_loginValidityDuration' - How long a sign-on session by a SAML user is valid, before the user has
-- to sign on again.
--
-- 'assertionAttributes', 'samlConfiguration_assertionAttributes' - A structure that defines which attributes in the SAML assertion are to
-- be used to define information about the users authenticated by that IdP
-- to use the workspace.
--
-- 'allowedOrganizations', 'samlConfiguration_allowedOrganizations' - Lists which organizations defined in the SAML assertion are allowed to
-- use the Amazon Managed Grafana workspace. If this is empty, all
-- organizations in the assertion attribute have access.
--
-- 'roleValues', 'samlConfiguration_roleValues' - A structure containing arrays that map group names in the SAML assertion
-- to the Grafana @Admin@ and @Editor@ roles in the workspace.
--
-- 'idpMetadata', 'samlConfiguration_idpMetadata' - A structure containing the identity provider (IdP) metadata used to
-- integrate the identity provider with this workspace.
newSamlConfiguration ::
  -- | 'idpMetadata'
  IdpMetadata ->
  SamlConfiguration
newSamlConfiguration pIdpMetadata_ =
  SamlConfiguration'
    { loginValidityDuration =
        Prelude.Nothing,
      assertionAttributes = Prelude.Nothing,
      allowedOrganizations = Prelude.Nothing,
      roleValues = Prelude.Nothing,
      idpMetadata = pIdpMetadata_
    }

-- | How long a sign-on session by a SAML user is valid, before the user has
-- to sign on again.
samlConfiguration_loginValidityDuration :: Lens.Lens' SamlConfiguration (Prelude.Maybe Prelude.Int)
samlConfiguration_loginValidityDuration = Lens.lens (\SamlConfiguration' {loginValidityDuration} -> loginValidityDuration) (\s@SamlConfiguration' {} a -> s {loginValidityDuration = a} :: SamlConfiguration)

-- | A structure that defines which attributes in the SAML assertion are to
-- be used to define information about the users authenticated by that IdP
-- to use the workspace.
samlConfiguration_assertionAttributes :: Lens.Lens' SamlConfiguration (Prelude.Maybe AssertionAttributes)
samlConfiguration_assertionAttributes = Lens.lens (\SamlConfiguration' {assertionAttributes} -> assertionAttributes) (\s@SamlConfiguration' {} a -> s {assertionAttributes = a} :: SamlConfiguration)

-- | Lists which organizations defined in the SAML assertion are allowed to
-- use the Amazon Managed Grafana workspace. If this is empty, all
-- organizations in the assertion attribute have access.
samlConfiguration_allowedOrganizations :: Lens.Lens' SamlConfiguration (Prelude.Maybe [Prelude.Text])
samlConfiguration_allowedOrganizations = Lens.lens (\SamlConfiguration' {allowedOrganizations} -> allowedOrganizations) (\s@SamlConfiguration' {} a -> s {allowedOrganizations = a} :: SamlConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | A structure containing arrays that map group names in the SAML assertion
-- to the Grafana @Admin@ and @Editor@ roles in the workspace.
samlConfiguration_roleValues :: Lens.Lens' SamlConfiguration (Prelude.Maybe RoleValues)
samlConfiguration_roleValues = Lens.lens (\SamlConfiguration' {roleValues} -> roleValues) (\s@SamlConfiguration' {} a -> s {roleValues = a} :: SamlConfiguration)

-- | A structure containing the identity provider (IdP) metadata used to
-- integrate the identity provider with this workspace.
samlConfiguration_idpMetadata :: Lens.Lens' SamlConfiguration IdpMetadata
samlConfiguration_idpMetadata = Lens.lens (\SamlConfiguration' {idpMetadata} -> idpMetadata) (\s@SamlConfiguration' {} a -> s {idpMetadata = a} :: SamlConfiguration)

instance Core.FromJSON SamlConfiguration where
  parseJSON =
    Core.withObject
      "SamlConfiguration"
      ( \x ->
          SamlConfiguration'
            Prelude.<$> (x Core..:? "loginValidityDuration")
            Prelude.<*> (x Core..:? "assertionAttributes")
            Prelude.<*> ( x Core..:? "allowedOrganizations"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "roleValues")
            Prelude.<*> (x Core..: "idpMetadata")
      )

instance Prelude.Hashable SamlConfiguration where
  hashWithSalt salt' SamlConfiguration' {..} =
    salt' `Prelude.hashWithSalt` idpMetadata
      `Prelude.hashWithSalt` roleValues
      `Prelude.hashWithSalt` allowedOrganizations
      `Prelude.hashWithSalt` assertionAttributes
      `Prelude.hashWithSalt` loginValidityDuration

instance Prelude.NFData SamlConfiguration where
  rnf SamlConfiguration' {..} =
    Prelude.rnf loginValidityDuration
      `Prelude.seq` Prelude.rnf idpMetadata
      `Prelude.seq` Prelude.rnf roleValues
      `Prelude.seq` Prelude.rnf allowedOrganizations
      `Prelude.seq` Prelude.rnf assertionAttributes

instance Core.ToJSON SamlConfiguration where
  toJSON SamlConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("loginValidityDuration" Core..=)
              Prelude.<$> loginValidityDuration,
            ("assertionAttributes" Core..=)
              Prelude.<$> assertionAttributes,
            ("allowedOrganizations" Core..=)
              Prelude.<$> allowedOrganizations,
            ("roleValues" Core..=) Prelude.<$> roleValues,
            Prelude.Just ("idpMetadata" Core..= idpMetadata)
          ]
      )
