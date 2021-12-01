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
-- Module      : Amazonka.ElasticBeanstalk.CreateApplication
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an application that has one configuration template named
-- @default@ and no application versions.
module Amazonka.ElasticBeanstalk.CreateApplication
  ( -- * Creating a Request
    CreateApplication (..),
    newCreateApplication,

    -- * Request Lenses
    createApplication_resourceLifecycleConfig,
    createApplication_description,
    createApplication_tags,
    createApplication_applicationName,

    -- * Destructuring the Response
    ApplicationDescriptionMessage (..),
    newApplicationDescriptionMessage,

    -- * Response Lenses
    applicationDescriptionMessage_application,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.ElasticBeanstalk.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Request to create an application.
--
-- /See:/ 'newCreateApplication' smart constructor.
data CreateApplication = CreateApplication'
  { -- | Specifies an application resource lifecycle configuration to prevent
    -- your application from accumulating too many versions.
    resourceLifecycleConfig :: Prelude.Maybe ApplicationResourceLifecycleConfig,
    -- | Your description of the application.
    description :: Prelude.Maybe Prelude.Text,
    -- | Specifies the tags applied to the application.
    --
    -- Elastic Beanstalk applies these tags only to the application.
    -- Environments that you create in the application don\'t inherit the tags.
    tags :: Prelude.Maybe [Tag],
    -- | The name of the application. Must be unique within your account.
    applicationName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateApplication' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceLifecycleConfig', 'createApplication_resourceLifecycleConfig' - Specifies an application resource lifecycle configuration to prevent
-- your application from accumulating too many versions.
--
-- 'description', 'createApplication_description' - Your description of the application.
--
-- 'tags', 'createApplication_tags' - Specifies the tags applied to the application.
--
-- Elastic Beanstalk applies these tags only to the application.
-- Environments that you create in the application don\'t inherit the tags.
--
-- 'applicationName', 'createApplication_applicationName' - The name of the application. Must be unique within your account.
newCreateApplication ::
  -- | 'applicationName'
  Prelude.Text ->
  CreateApplication
newCreateApplication pApplicationName_ =
  CreateApplication'
    { resourceLifecycleConfig =
        Prelude.Nothing,
      description = Prelude.Nothing,
      tags = Prelude.Nothing,
      applicationName = pApplicationName_
    }

-- | Specifies an application resource lifecycle configuration to prevent
-- your application from accumulating too many versions.
createApplication_resourceLifecycleConfig :: Lens.Lens' CreateApplication (Prelude.Maybe ApplicationResourceLifecycleConfig)
createApplication_resourceLifecycleConfig = Lens.lens (\CreateApplication' {resourceLifecycleConfig} -> resourceLifecycleConfig) (\s@CreateApplication' {} a -> s {resourceLifecycleConfig = a} :: CreateApplication)

-- | Your description of the application.
createApplication_description :: Lens.Lens' CreateApplication (Prelude.Maybe Prelude.Text)
createApplication_description = Lens.lens (\CreateApplication' {description} -> description) (\s@CreateApplication' {} a -> s {description = a} :: CreateApplication)

-- | Specifies the tags applied to the application.
--
-- Elastic Beanstalk applies these tags only to the application.
-- Environments that you create in the application don\'t inherit the tags.
createApplication_tags :: Lens.Lens' CreateApplication (Prelude.Maybe [Tag])
createApplication_tags = Lens.lens (\CreateApplication' {tags} -> tags) (\s@CreateApplication' {} a -> s {tags = a} :: CreateApplication) Prelude.. Lens.mapping Lens.coerced

-- | The name of the application. Must be unique within your account.
createApplication_applicationName :: Lens.Lens' CreateApplication Prelude.Text
createApplication_applicationName = Lens.lens (\CreateApplication' {applicationName} -> applicationName) (\s@CreateApplication' {} a -> s {applicationName = a} :: CreateApplication)

instance Core.AWSRequest CreateApplication where
  type
    AWSResponse CreateApplication =
      ApplicationDescriptionMessage
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "CreateApplicationResult"
      (\s h x -> Core.parseXML x)

instance Prelude.Hashable CreateApplication where
  hashWithSalt salt' CreateApplication' {..} =
    salt' `Prelude.hashWithSalt` applicationName
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` resourceLifecycleConfig

instance Prelude.NFData CreateApplication where
  rnf CreateApplication' {..} =
    Prelude.rnf resourceLifecycleConfig
      `Prelude.seq` Prelude.rnf applicationName
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf description

instance Core.ToHeaders CreateApplication where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath CreateApplication where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateApplication where
  toQuery CreateApplication' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("CreateApplication" :: Prelude.ByteString),
        "Version"
          Core.=: ("2010-12-01" :: Prelude.ByteString),
        "ResourceLifecycleConfig"
          Core.=: resourceLifecycleConfig,
        "Description" Core.=: description,
        "Tags"
          Core.=: Core.toQuery
            (Core.toQueryList "member" Prelude.<$> tags),
        "ApplicationName" Core.=: applicationName
      ]
