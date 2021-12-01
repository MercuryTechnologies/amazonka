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
-- Module      : Amazonka.ECS.Types.FSxWindowsFileServerAuthorizationConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECS.Types.FSxWindowsFileServerAuthorizationConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The authorization configuration details for Amazon FSx for Windows File
-- Server file system. See
-- <https://docs.aws.amazon.com/AmazonECS/latest/APIReference/API_FSxWindowsFileServerVolumeConfiguration.html FSxWindowsFileServerVolumeConfiguration>
-- in the /Amazon Elastic Container Service API Reference/.
--
-- For more information and the input format, see
-- <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/wfsx-volumes.html Amazon FSx for Windows File Server Volumes>
-- in the /Amazon Elastic Container Service Developer Guide/.
--
-- /See:/ 'newFSxWindowsFileServerAuthorizationConfig' smart constructor.
data FSxWindowsFileServerAuthorizationConfig = FSxWindowsFileServerAuthorizationConfig'
  { -- | The authorization credential option to use. The authorization credential
    -- options can be provided using either the Amazon Resource Name (ARN) of
    -- an Secrets Manager secret or SSM Parameter Store parameter. The ARNs
    -- refer to the stored credentials.
    credentialsParameter :: Prelude.Text,
    -- | A fully qualified domain name hosted by an
    -- <https://docs.aws.amazon.com/directoryservice/latest/admin-guide/directory_microsoft_ad.html Directory Service>
    -- Managed Microsoft AD (Active Directory) or self-hosted AD on Amazon EC2.
    domain :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FSxWindowsFileServerAuthorizationConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'credentialsParameter', 'fSxWindowsFileServerAuthorizationConfig_credentialsParameter' - The authorization credential option to use. The authorization credential
-- options can be provided using either the Amazon Resource Name (ARN) of
-- an Secrets Manager secret or SSM Parameter Store parameter. The ARNs
-- refer to the stored credentials.
--
-- 'domain', 'fSxWindowsFileServerAuthorizationConfig_domain' - A fully qualified domain name hosted by an
-- <https://docs.aws.amazon.com/directoryservice/latest/admin-guide/directory_microsoft_ad.html Directory Service>
-- Managed Microsoft AD (Active Directory) or self-hosted AD on Amazon EC2.
newFSxWindowsFileServerAuthorizationConfig ::
  -- | 'credentialsParameter'
  Prelude.Text ->
  -- | 'domain'
  Prelude.Text ->
  FSxWindowsFileServerAuthorizationConfig
newFSxWindowsFileServerAuthorizationConfig
  pCredentialsParameter_
  pDomain_ =
    FSxWindowsFileServerAuthorizationConfig'
      { credentialsParameter =
          pCredentialsParameter_,
        domain = pDomain_
      }

-- | The authorization credential option to use. The authorization credential
-- options can be provided using either the Amazon Resource Name (ARN) of
-- an Secrets Manager secret or SSM Parameter Store parameter. The ARNs
-- refer to the stored credentials.
fSxWindowsFileServerAuthorizationConfig_credentialsParameter :: Lens.Lens' FSxWindowsFileServerAuthorizationConfig Prelude.Text
fSxWindowsFileServerAuthorizationConfig_credentialsParameter = Lens.lens (\FSxWindowsFileServerAuthorizationConfig' {credentialsParameter} -> credentialsParameter) (\s@FSxWindowsFileServerAuthorizationConfig' {} a -> s {credentialsParameter = a} :: FSxWindowsFileServerAuthorizationConfig)

-- | A fully qualified domain name hosted by an
-- <https://docs.aws.amazon.com/directoryservice/latest/admin-guide/directory_microsoft_ad.html Directory Service>
-- Managed Microsoft AD (Active Directory) or self-hosted AD on Amazon EC2.
fSxWindowsFileServerAuthorizationConfig_domain :: Lens.Lens' FSxWindowsFileServerAuthorizationConfig Prelude.Text
fSxWindowsFileServerAuthorizationConfig_domain = Lens.lens (\FSxWindowsFileServerAuthorizationConfig' {domain} -> domain) (\s@FSxWindowsFileServerAuthorizationConfig' {} a -> s {domain = a} :: FSxWindowsFileServerAuthorizationConfig)

instance
  Core.FromJSON
    FSxWindowsFileServerAuthorizationConfig
  where
  parseJSON =
    Core.withObject
      "FSxWindowsFileServerAuthorizationConfig"
      ( \x ->
          FSxWindowsFileServerAuthorizationConfig'
            Prelude.<$> (x Core..: "credentialsParameter")
            Prelude.<*> (x Core..: "domain")
      )

instance
  Prelude.Hashable
    FSxWindowsFileServerAuthorizationConfig
  where
  hashWithSalt
    salt'
    FSxWindowsFileServerAuthorizationConfig' {..} =
      salt' `Prelude.hashWithSalt` domain
        `Prelude.hashWithSalt` credentialsParameter

instance
  Prelude.NFData
    FSxWindowsFileServerAuthorizationConfig
  where
  rnf FSxWindowsFileServerAuthorizationConfig' {..} =
    Prelude.rnf credentialsParameter
      `Prelude.seq` Prelude.rnf domain

instance
  Core.ToJSON
    FSxWindowsFileServerAuthorizationConfig
  where
  toJSON FSxWindowsFileServerAuthorizationConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "credentialsParameter"
                  Core..= credentialsParameter
              ),
            Prelude.Just ("domain" Core..= domain)
          ]
      )
