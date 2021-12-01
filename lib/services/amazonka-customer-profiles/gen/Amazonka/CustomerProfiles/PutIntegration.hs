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
-- Module      : Amazonka.CustomerProfiles.PutIntegration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds an integration between the service and a third-party service, which
-- includes Amazon AppFlow and Amazon Connect.
--
-- An integration can belong to only one domain.
module Amazonka.CustomerProfiles.PutIntegration
  ( -- * Creating a Request
    PutIntegration (..),
    newPutIntegration,

    -- * Request Lenses
    putIntegration_flowDefinition,
    putIntegration_uri,
    putIntegration_tags,
    putIntegration_domainName,
    putIntegration_objectTypeName,

    -- * Destructuring the Response
    PutIntegrationResponse (..),
    newPutIntegrationResponse,

    -- * Response Lenses
    putIntegrationResponse_tags,
    putIntegrationResponse_httpStatus,
    putIntegrationResponse_domainName,
    putIntegrationResponse_uri,
    putIntegrationResponse_objectTypeName,
    putIntegrationResponse_createdAt,
    putIntegrationResponse_lastUpdatedAt,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.CustomerProfiles.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutIntegration' smart constructor.
data PutIntegration = PutIntegration'
  { -- | The configuration that controls how Customer Profiles retrieves data
    -- from the source.
    flowDefinition :: Prelude.Maybe FlowDefinition,
    -- | The URI of the S3 bucket or any other type of data source.
    uri :: Prelude.Maybe Prelude.Text,
    -- | The tags used to organize, track, or control access for this resource.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The unique name of the domain.
    domainName :: Prelude.Text,
    -- | The name of the profile object type.
    objectTypeName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutIntegration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'flowDefinition', 'putIntegration_flowDefinition' - The configuration that controls how Customer Profiles retrieves data
-- from the source.
--
-- 'uri', 'putIntegration_uri' - The URI of the S3 bucket or any other type of data source.
--
-- 'tags', 'putIntegration_tags' - The tags used to organize, track, or control access for this resource.
--
-- 'domainName', 'putIntegration_domainName' - The unique name of the domain.
--
-- 'objectTypeName', 'putIntegration_objectTypeName' - The name of the profile object type.
newPutIntegration ::
  -- | 'domainName'
  Prelude.Text ->
  -- | 'objectTypeName'
  Prelude.Text ->
  PutIntegration
newPutIntegration pDomainName_ pObjectTypeName_ =
  PutIntegration'
    { flowDefinition = Prelude.Nothing,
      uri = Prelude.Nothing,
      tags = Prelude.Nothing,
      domainName = pDomainName_,
      objectTypeName = pObjectTypeName_
    }

-- | The configuration that controls how Customer Profiles retrieves data
-- from the source.
putIntegration_flowDefinition :: Lens.Lens' PutIntegration (Prelude.Maybe FlowDefinition)
putIntegration_flowDefinition = Lens.lens (\PutIntegration' {flowDefinition} -> flowDefinition) (\s@PutIntegration' {} a -> s {flowDefinition = a} :: PutIntegration)

-- | The URI of the S3 bucket or any other type of data source.
putIntegration_uri :: Lens.Lens' PutIntegration (Prelude.Maybe Prelude.Text)
putIntegration_uri = Lens.lens (\PutIntegration' {uri} -> uri) (\s@PutIntegration' {} a -> s {uri = a} :: PutIntegration)

-- | The tags used to organize, track, or control access for this resource.
putIntegration_tags :: Lens.Lens' PutIntegration (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
putIntegration_tags = Lens.lens (\PutIntegration' {tags} -> tags) (\s@PutIntegration' {} a -> s {tags = a} :: PutIntegration) Prelude.. Lens.mapping Lens.coerced

-- | The unique name of the domain.
putIntegration_domainName :: Lens.Lens' PutIntegration Prelude.Text
putIntegration_domainName = Lens.lens (\PutIntegration' {domainName} -> domainName) (\s@PutIntegration' {} a -> s {domainName = a} :: PutIntegration)

-- | The name of the profile object type.
putIntegration_objectTypeName :: Lens.Lens' PutIntegration Prelude.Text
putIntegration_objectTypeName = Lens.lens (\PutIntegration' {objectTypeName} -> objectTypeName) (\s@PutIntegration' {} a -> s {objectTypeName = a} :: PutIntegration)

instance Core.AWSRequest PutIntegration where
  type
    AWSResponse PutIntegration =
      PutIntegrationResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          PutIntegrationResponse'
            Prelude.<$> (x Core..?> "Tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "DomainName")
            Prelude.<*> (x Core..:> "Uri")
            Prelude.<*> (x Core..:> "ObjectTypeName")
            Prelude.<*> (x Core..:> "CreatedAt")
            Prelude.<*> (x Core..:> "LastUpdatedAt")
      )

instance Prelude.Hashable PutIntegration where
  hashWithSalt salt' PutIntegration' {..} =
    salt' `Prelude.hashWithSalt` objectTypeName
      `Prelude.hashWithSalt` domainName
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` uri
      `Prelude.hashWithSalt` flowDefinition

instance Prelude.NFData PutIntegration where
  rnf PutIntegration' {..} =
    Prelude.rnf flowDefinition
      `Prelude.seq` Prelude.rnf objectTypeName
      `Prelude.seq` Prelude.rnf domainName
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf uri

instance Core.ToHeaders PutIntegration where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON PutIntegration where
  toJSON PutIntegration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("FlowDefinition" Core..=)
              Prelude.<$> flowDefinition,
            ("Uri" Core..=) Prelude.<$> uri,
            ("Tags" Core..=) Prelude.<$> tags,
            Prelude.Just
              ("ObjectTypeName" Core..= objectTypeName)
          ]
      )

instance Core.ToPath PutIntegration where
  toPath PutIntegration' {..} =
    Prelude.mconcat
      ["/domains/", Core.toBS domainName, "/integrations"]

instance Core.ToQuery PutIntegration where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutIntegrationResponse' smart constructor.
data PutIntegrationResponse = PutIntegrationResponse'
  { -- | The tags used to organize, track, or control access for this resource.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The unique name of the domain.
    domainName :: Prelude.Text,
    -- | The URI of the S3 bucket or any other type of data source.
    uri :: Prelude.Text,
    -- | The name of the profile object type.
    objectTypeName :: Prelude.Text,
    -- | The timestamp of when the domain was created.
    createdAt :: Core.POSIX,
    -- | The timestamp of when the domain was most recently edited.
    lastUpdatedAt :: Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutIntegrationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'putIntegrationResponse_tags' - The tags used to organize, track, or control access for this resource.
--
-- 'httpStatus', 'putIntegrationResponse_httpStatus' - The response's http status code.
--
-- 'domainName', 'putIntegrationResponse_domainName' - The unique name of the domain.
--
-- 'uri', 'putIntegrationResponse_uri' - The URI of the S3 bucket or any other type of data source.
--
-- 'objectTypeName', 'putIntegrationResponse_objectTypeName' - The name of the profile object type.
--
-- 'createdAt', 'putIntegrationResponse_createdAt' - The timestamp of when the domain was created.
--
-- 'lastUpdatedAt', 'putIntegrationResponse_lastUpdatedAt' - The timestamp of when the domain was most recently edited.
newPutIntegrationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'domainName'
  Prelude.Text ->
  -- | 'uri'
  Prelude.Text ->
  -- | 'objectTypeName'
  Prelude.Text ->
  -- | 'createdAt'
  Prelude.UTCTime ->
  -- | 'lastUpdatedAt'
  Prelude.UTCTime ->
  PutIntegrationResponse
newPutIntegrationResponse
  pHttpStatus_
  pDomainName_
  pUri_
  pObjectTypeName_
  pCreatedAt_
  pLastUpdatedAt_ =
    PutIntegrationResponse'
      { tags = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        domainName = pDomainName_,
        uri = pUri_,
        objectTypeName = pObjectTypeName_,
        createdAt = Core._Time Lens.# pCreatedAt_,
        lastUpdatedAt = Core._Time Lens.# pLastUpdatedAt_
      }

-- | The tags used to organize, track, or control access for this resource.
putIntegrationResponse_tags :: Lens.Lens' PutIntegrationResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
putIntegrationResponse_tags = Lens.lens (\PutIntegrationResponse' {tags} -> tags) (\s@PutIntegrationResponse' {} a -> s {tags = a} :: PutIntegrationResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
putIntegrationResponse_httpStatus :: Lens.Lens' PutIntegrationResponse Prelude.Int
putIntegrationResponse_httpStatus = Lens.lens (\PutIntegrationResponse' {httpStatus} -> httpStatus) (\s@PutIntegrationResponse' {} a -> s {httpStatus = a} :: PutIntegrationResponse)

-- | The unique name of the domain.
putIntegrationResponse_domainName :: Lens.Lens' PutIntegrationResponse Prelude.Text
putIntegrationResponse_domainName = Lens.lens (\PutIntegrationResponse' {domainName} -> domainName) (\s@PutIntegrationResponse' {} a -> s {domainName = a} :: PutIntegrationResponse)

-- | The URI of the S3 bucket or any other type of data source.
putIntegrationResponse_uri :: Lens.Lens' PutIntegrationResponse Prelude.Text
putIntegrationResponse_uri = Lens.lens (\PutIntegrationResponse' {uri} -> uri) (\s@PutIntegrationResponse' {} a -> s {uri = a} :: PutIntegrationResponse)

-- | The name of the profile object type.
putIntegrationResponse_objectTypeName :: Lens.Lens' PutIntegrationResponse Prelude.Text
putIntegrationResponse_objectTypeName = Lens.lens (\PutIntegrationResponse' {objectTypeName} -> objectTypeName) (\s@PutIntegrationResponse' {} a -> s {objectTypeName = a} :: PutIntegrationResponse)

-- | The timestamp of when the domain was created.
putIntegrationResponse_createdAt :: Lens.Lens' PutIntegrationResponse Prelude.UTCTime
putIntegrationResponse_createdAt = Lens.lens (\PutIntegrationResponse' {createdAt} -> createdAt) (\s@PutIntegrationResponse' {} a -> s {createdAt = a} :: PutIntegrationResponse) Prelude.. Core._Time

-- | The timestamp of when the domain was most recently edited.
putIntegrationResponse_lastUpdatedAt :: Lens.Lens' PutIntegrationResponse Prelude.UTCTime
putIntegrationResponse_lastUpdatedAt = Lens.lens (\PutIntegrationResponse' {lastUpdatedAt} -> lastUpdatedAt) (\s@PutIntegrationResponse' {} a -> s {lastUpdatedAt = a} :: PutIntegrationResponse) Prelude.. Core._Time

instance Prelude.NFData PutIntegrationResponse where
  rnf PutIntegrationResponse' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf lastUpdatedAt
      `Prelude.seq` Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf objectTypeName
      `Prelude.seq` Prelude.rnf uri
      `Prelude.seq` Prelude.rnf domainName
      `Prelude.seq` Prelude.rnf httpStatus
