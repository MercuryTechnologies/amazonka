{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.AppConfig.UpdateConfigurationProfile
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a configuration profile.
module Amazonka.AppConfig.UpdateConfigurationProfile
  ( -- * Creating a Request
    UpdateConfigurationProfile (..),
    newUpdateConfigurationProfile,

    -- * Request Lenses
    updateConfigurationProfile_retrievalRoleArn,
    updateConfigurationProfile_validators,
    updateConfigurationProfile_name,
    updateConfigurationProfile_description,
    updateConfigurationProfile_applicationId,
    updateConfigurationProfile_configurationProfileId,

    -- * Destructuring the Response
    ConfigurationProfile (..),
    newConfigurationProfile,

    -- * Response Lenses
    configurationProfile_retrievalRoleArn,
    configurationProfile_validators,
    configurationProfile_locationUri,
    configurationProfile_applicationId,
    configurationProfile_name,
    configurationProfile_id,
    configurationProfile_description,
  )
where

import Amazonka.AppConfig.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateConfigurationProfile' smart constructor.
data UpdateConfigurationProfile = UpdateConfigurationProfile'
  { -- | The ARN of an IAM role with permission to access the configuration at
    -- the specified LocationUri.
    retrievalRoleArn :: Prelude.Maybe Prelude.Text,
    -- | A list of methods for validating the configuration.
    validators :: Prelude.Maybe [Validator],
    -- | The name of the configuration profile.
    name :: Prelude.Maybe Prelude.Text,
    -- | A description of the configuration profile.
    description :: Prelude.Maybe Prelude.Text,
    -- | The application ID.
    applicationId :: Prelude.Text,
    -- | The ID of the configuration profile.
    configurationProfileId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateConfigurationProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'retrievalRoleArn', 'updateConfigurationProfile_retrievalRoleArn' - The ARN of an IAM role with permission to access the configuration at
-- the specified LocationUri.
--
-- 'validators', 'updateConfigurationProfile_validators' - A list of methods for validating the configuration.
--
-- 'name', 'updateConfigurationProfile_name' - The name of the configuration profile.
--
-- 'description', 'updateConfigurationProfile_description' - A description of the configuration profile.
--
-- 'applicationId', 'updateConfigurationProfile_applicationId' - The application ID.
--
-- 'configurationProfileId', 'updateConfigurationProfile_configurationProfileId' - The ID of the configuration profile.
newUpdateConfigurationProfile ::
  -- | 'applicationId'
  Prelude.Text ->
  -- | 'configurationProfileId'
  Prelude.Text ->
  UpdateConfigurationProfile
newUpdateConfigurationProfile
  pApplicationId_
  pConfigurationProfileId_ =
    UpdateConfigurationProfile'
      { retrievalRoleArn =
          Prelude.Nothing,
        validators = Prelude.Nothing,
        name = Prelude.Nothing,
        description = Prelude.Nothing,
        applicationId = pApplicationId_,
        configurationProfileId =
          pConfigurationProfileId_
      }

-- | The ARN of an IAM role with permission to access the configuration at
-- the specified LocationUri.
updateConfigurationProfile_retrievalRoleArn :: Lens.Lens' UpdateConfigurationProfile (Prelude.Maybe Prelude.Text)
updateConfigurationProfile_retrievalRoleArn = Lens.lens (\UpdateConfigurationProfile' {retrievalRoleArn} -> retrievalRoleArn) (\s@UpdateConfigurationProfile' {} a -> s {retrievalRoleArn = a} :: UpdateConfigurationProfile)

-- | A list of methods for validating the configuration.
updateConfigurationProfile_validators :: Lens.Lens' UpdateConfigurationProfile (Prelude.Maybe [Validator])
updateConfigurationProfile_validators = Lens.lens (\UpdateConfigurationProfile' {validators} -> validators) (\s@UpdateConfigurationProfile' {} a -> s {validators = a} :: UpdateConfigurationProfile) Prelude.. Lens.mapping Lens.coerced

-- | The name of the configuration profile.
updateConfigurationProfile_name :: Lens.Lens' UpdateConfigurationProfile (Prelude.Maybe Prelude.Text)
updateConfigurationProfile_name = Lens.lens (\UpdateConfigurationProfile' {name} -> name) (\s@UpdateConfigurationProfile' {} a -> s {name = a} :: UpdateConfigurationProfile)

-- | A description of the configuration profile.
updateConfigurationProfile_description :: Lens.Lens' UpdateConfigurationProfile (Prelude.Maybe Prelude.Text)
updateConfigurationProfile_description = Lens.lens (\UpdateConfigurationProfile' {description} -> description) (\s@UpdateConfigurationProfile' {} a -> s {description = a} :: UpdateConfigurationProfile)

-- | The application ID.
updateConfigurationProfile_applicationId :: Lens.Lens' UpdateConfigurationProfile Prelude.Text
updateConfigurationProfile_applicationId = Lens.lens (\UpdateConfigurationProfile' {applicationId} -> applicationId) (\s@UpdateConfigurationProfile' {} a -> s {applicationId = a} :: UpdateConfigurationProfile)

-- | The ID of the configuration profile.
updateConfigurationProfile_configurationProfileId :: Lens.Lens' UpdateConfigurationProfile Prelude.Text
updateConfigurationProfile_configurationProfileId = Lens.lens (\UpdateConfigurationProfile' {configurationProfileId} -> configurationProfileId) (\s@UpdateConfigurationProfile' {} a -> s {configurationProfileId = a} :: UpdateConfigurationProfile)

instance Core.AWSRequest UpdateConfigurationProfile where
  type
    AWSResponse UpdateConfigurationProfile =
      ConfigurationProfile
  request = Request.patchJSON defaultService
  response =
    Response.receiveJSON
      (\s h x -> Core.eitherParseJSON x)

instance Prelude.Hashable UpdateConfigurationProfile where
  hashWithSalt salt' UpdateConfigurationProfile' {..} =
    salt' `Prelude.hashWithSalt` configurationProfileId
      `Prelude.hashWithSalt` applicationId
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` validators
      `Prelude.hashWithSalt` retrievalRoleArn

instance Prelude.NFData UpdateConfigurationProfile where
  rnf UpdateConfigurationProfile' {..} =
    Prelude.rnf retrievalRoleArn
      `Prelude.seq` Prelude.rnf configurationProfileId
      `Prelude.seq` Prelude.rnf applicationId
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf validators

instance Core.ToHeaders UpdateConfigurationProfile where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateConfigurationProfile where
  toJSON UpdateConfigurationProfile' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("RetrievalRoleArn" Core..=)
              Prelude.<$> retrievalRoleArn,
            ("Validators" Core..=) Prelude.<$> validators,
            ("Name" Core..=) Prelude.<$> name,
            ("Description" Core..=) Prelude.<$> description
          ]
      )

instance Core.ToPath UpdateConfigurationProfile where
  toPath UpdateConfigurationProfile' {..} =
    Prelude.mconcat
      [ "/applications/",
        Core.toBS applicationId,
        "/configurationprofiles/",
        Core.toBS configurationProfileId
      ]

instance Core.ToQuery UpdateConfigurationProfile where
  toQuery = Prelude.const Prelude.mempty
