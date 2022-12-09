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
-- Module      : Amazonka.IAM.CreateSAMLProvider
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an IAM resource that describes an identity provider (IdP) that
-- supports SAML 2.0.
--
-- The SAML provider resource that you create with this operation can be
-- used as a principal in an IAM role\'s trust policy. Such a policy can
-- enable federated users who sign in using the SAML IdP to assume the
-- role. You can create an IAM role that supports Web-based single sign-on
-- (SSO) to the Amazon Web Services Management Console or one that supports
-- API access to Amazon Web Services.
--
-- When you create the SAML provider resource, you upload a SAML metadata
-- document that you get from your IdP. That document includes the
-- issuer\'s name, expiration information, and keys that can be used to
-- validate the SAML authentication response (assertions) that the IdP
-- sends. You must generate the metadata document using the identity
-- management software that is used as your organization\'s IdP.
--
-- This operation requires
-- <https://docs.aws.amazon.com/general/latest/gr/signature-version-4.html Signature Version 4>.
--
-- For more information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_providers_enable-console-saml.html Enabling SAML 2.0 federated users to access the Amazon Web Services Management Console>
-- and
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_providers_saml.html About SAML 2.0-based federation>
-- in the /IAM User Guide/.
module Amazonka.IAM.CreateSAMLProvider
  ( -- * Creating a Request
    CreateSAMLProvider (..),
    newCreateSAMLProvider,

    -- * Request Lenses
    createSAMLProvider_tags,
    createSAMLProvider_sAMLMetadataDocument,
    createSAMLProvider_name,

    -- * Destructuring the Response
    CreateSAMLProviderResponse (..),
    newCreateSAMLProviderResponse,

    -- * Response Lenses
    createSAMLProviderResponse_sAMLProviderArn,
    createSAMLProviderResponse_tags,
    createSAMLProviderResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IAM.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateSAMLProvider' smart constructor.
data CreateSAMLProvider = CreateSAMLProvider'
  { -- | A list of tags that you want to attach to the new IAM SAML provider.
    -- Each tag consists of a key name and an associated value. For more
    -- information about tagging, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM resources>
    -- in the /IAM User Guide/.
    --
    -- If any one of the tags is invalid or if you exceed the allowed maximum
    -- number of tags, then the entire request fails and the resource is not
    -- created.
    tags :: Prelude.Maybe [Tag],
    -- | An XML document generated by an identity provider (IdP) that supports
    -- SAML 2.0. The document includes the issuer\'s name, expiration
    -- information, and keys that can be used to validate the SAML
    -- authentication response (assertions) that are received from the IdP. You
    -- must generate the metadata document using the identity management
    -- software that is used as your organization\'s IdP.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_providers_saml.html About SAML 2.0-based federation>
    -- in the /IAM User Guide/
    sAMLMetadataDocument :: Prelude.Text,
    -- | The name of the provider to create.
    --
    -- This parameter allows (through its
    -- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
    -- consisting of upper and lowercase alphanumeric characters with no
    -- spaces. You can also include any of the following characters: _+=,.\@-
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateSAMLProvider' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createSAMLProvider_tags' - A list of tags that you want to attach to the new IAM SAML provider.
-- Each tag consists of a key name and an associated value. For more
-- information about tagging, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM resources>
-- in the /IAM User Guide/.
--
-- If any one of the tags is invalid or if you exceed the allowed maximum
-- number of tags, then the entire request fails and the resource is not
-- created.
--
-- 'sAMLMetadataDocument', 'createSAMLProvider_sAMLMetadataDocument' - An XML document generated by an identity provider (IdP) that supports
-- SAML 2.0. The document includes the issuer\'s name, expiration
-- information, and keys that can be used to validate the SAML
-- authentication response (assertions) that are received from the IdP. You
-- must generate the metadata document using the identity management
-- software that is used as your organization\'s IdP.
--
-- For more information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_providers_saml.html About SAML 2.0-based federation>
-- in the /IAM User Guide/
--
-- 'name', 'createSAMLProvider_name' - The name of the provider to create.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
newCreateSAMLProvider ::
  -- | 'sAMLMetadataDocument'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  CreateSAMLProvider
newCreateSAMLProvider pSAMLMetadataDocument_ pName_ =
  CreateSAMLProvider'
    { tags = Prelude.Nothing,
      sAMLMetadataDocument = pSAMLMetadataDocument_,
      name = pName_
    }

-- | A list of tags that you want to attach to the new IAM SAML provider.
-- Each tag consists of a key name and an associated value. For more
-- information about tagging, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM resources>
-- in the /IAM User Guide/.
--
-- If any one of the tags is invalid or if you exceed the allowed maximum
-- number of tags, then the entire request fails and the resource is not
-- created.
createSAMLProvider_tags :: Lens.Lens' CreateSAMLProvider (Prelude.Maybe [Tag])
createSAMLProvider_tags = Lens.lens (\CreateSAMLProvider' {tags} -> tags) (\s@CreateSAMLProvider' {} a -> s {tags = a} :: CreateSAMLProvider) Prelude.. Lens.mapping Lens.coerced

-- | An XML document generated by an identity provider (IdP) that supports
-- SAML 2.0. The document includes the issuer\'s name, expiration
-- information, and keys that can be used to validate the SAML
-- authentication response (assertions) that are received from the IdP. You
-- must generate the metadata document using the identity management
-- software that is used as your organization\'s IdP.
--
-- For more information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_providers_saml.html About SAML 2.0-based federation>
-- in the /IAM User Guide/
createSAMLProvider_sAMLMetadataDocument :: Lens.Lens' CreateSAMLProvider Prelude.Text
createSAMLProvider_sAMLMetadataDocument = Lens.lens (\CreateSAMLProvider' {sAMLMetadataDocument} -> sAMLMetadataDocument) (\s@CreateSAMLProvider' {} a -> s {sAMLMetadataDocument = a} :: CreateSAMLProvider)

-- | The name of the provider to create.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
createSAMLProvider_name :: Lens.Lens' CreateSAMLProvider Prelude.Text
createSAMLProvider_name = Lens.lens (\CreateSAMLProvider' {name} -> name) (\s@CreateSAMLProvider' {} a -> s {name = a} :: CreateSAMLProvider)

instance Core.AWSRequest CreateSAMLProvider where
  type
    AWSResponse CreateSAMLProvider =
      CreateSAMLProviderResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "CreateSAMLProviderResult"
      ( \s h x ->
          CreateSAMLProviderResponse'
            Prelude.<$> (x Data..@? "SAMLProviderArn")
            Prelude.<*> ( x Data..@? "Tags" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "member")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateSAMLProvider where
  hashWithSalt _salt CreateSAMLProvider' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` sAMLMetadataDocument
      `Prelude.hashWithSalt` name

instance Prelude.NFData CreateSAMLProvider where
  rnf CreateSAMLProvider' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf sAMLMetadataDocument
      `Prelude.seq` Prelude.rnf name

instance Data.ToHeaders CreateSAMLProvider where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath CreateSAMLProvider where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateSAMLProvider where
  toQuery CreateSAMLProvider' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("CreateSAMLProvider" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-05-08" :: Prelude.ByteString),
        "Tags"
          Data.=: Data.toQuery
            (Data.toQueryList "member" Prelude.<$> tags),
        "SAMLMetadataDocument" Data.=: sAMLMetadataDocument,
        "Name" Data.=: name
      ]

-- | Contains the response to a successful CreateSAMLProvider request.
--
-- /See:/ 'newCreateSAMLProviderResponse' smart constructor.
data CreateSAMLProviderResponse = CreateSAMLProviderResponse'
  { -- | The Amazon Resource Name (ARN) of the new SAML provider resource in IAM.
    sAMLProviderArn :: Prelude.Maybe Prelude.Text,
    -- | A list of tags that are attached to the new IAM SAML provider. The
    -- returned list of tags is sorted by tag key. For more information about
    -- tagging, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM resources>
    -- in the /IAM User Guide/.
    tags :: Prelude.Maybe [Tag],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateSAMLProviderResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sAMLProviderArn', 'createSAMLProviderResponse_sAMLProviderArn' - The Amazon Resource Name (ARN) of the new SAML provider resource in IAM.
--
-- 'tags', 'createSAMLProviderResponse_tags' - A list of tags that are attached to the new IAM SAML provider. The
-- returned list of tags is sorted by tag key. For more information about
-- tagging, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM resources>
-- in the /IAM User Guide/.
--
-- 'httpStatus', 'createSAMLProviderResponse_httpStatus' - The response's http status code.
newCreateSAMLProviderResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateSAMLProviderResponse
newCreateSAMLProviderResponse pHttpStatus_ =
  CreateSAMLProviderResponse'
    { sAMLProviderArn =
        Prelude.Nothing,
      tags = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the new SAML provider resource in IAM.
createSAMLProviderResponse_sAMLProviderArn :: Lens.Lens' CreateSAMLProviderResponse (Prelude.Maybe Prelude.Text)
createSAMLProviderResponse_sAMLProviderArn = Lens.lens (\CreateSAMLProviderResponse' {sAMLProviderArn} -> sAMLProviderArn) (\s@CreateSAMLProviderResponse' {} a -> s {sAMLProviderArn = a} :: CreateSAMLProviderResponse)

-- | A list of tags that are attached to the new IAM SAML provider. The
-- returned list of tags is sorted by tag key. For more information about
-- tagging, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM resources>
-- in the /IAM User Guide/.
createSAMLProviderResponse_tags :: Lens.Lens' CreateSAMLProviderResponse (Prelude.Maybe [Tag])
createSAMLProviderResponse_tags = Lens.lens (\CreateSAMLProviderResponse' {tags} -> tags) (\s@CreateSAMLProviderResponse' {} a -> s {tags = a} :: CreateSAMLProviderResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
createSAMLProviderResponse_httpStatus :: Lens.Lens' CreateSAMLProviderResponse Prelude.Int
createSAMLProviderResponse_httpStatus = Lens.lens (\CreateSAMLProviderResponse' {httpStatus} -> httpStatus) (\s@CreateSAMLProviderResponse' {} a -> s {httpStatus = a} :: CreateSAMLProviderResponse)

instance Prelude.NFData CreateSAMLProviderResponse where
  rnf CreateSAMLProviderResponse' {..} =
    Prelude.rnf sAMLProviderArn
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf httpStatus
