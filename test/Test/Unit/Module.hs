{-# LANGUAGE CPP #-}
module Test.Unit.Module (
    tests
  ) where

import Test.Unit.AuthToken (encryptionDecryptionIsomorphism)

import Test.Tasty.TestSet (group)

import Bead.Config.Parser (parseTests)
import Bead.Controller.Pages (pageDescTest)
import Bead.Domain.Entities (asgTests, entityTests, feedbackTests)
import Bead.Domain.Relationships (relationshipTests)
import Bead.Domain.RolePermission (permissionTest)
import Bead.Persistence.Persist (persistTests)
import Bead.Persistence.Relations (persistRelationsTests)
import Bead.View.Content.All (pageContentTest)
import Bead.View.DataBridge (dataBridgeTests)
import Bead.View.Dictionary (patchDictionariesTests)
#ifdef EmailEnabled
import Bead.View.EmailTemplate (runEmailTemplateTests)
#endif
import Bead.View.Header (acceptLanguageTests)
import Bead.View.RouteOf (routeOfTest)
import Bead.View.Routing (routingTest)
import Bead.View.TemplateAndComponentNames (fieldNameTest)
#ifdef EmailEnabled
import Bead.View.Validators (emailAddressTests)
#endif

tests = do
  group "Authentication" encryptionDecryptionIsomorphism
  group "Page description" pageDescTest
  group "Route of" routeOfTest
  group "Routing" routingTest
  group "Page content handler " pageContentTest
  group "Permissions" permissionTest
  group "Field name" fieldNameTest
  group "Entity" entityTests
  group "Relationships" relationshipTests
  group "Assignment" asgTests
  group "Persist" persistTests
#ifdef EmailEnabled
  group "Email address" emailAddressTests
  group "Run email template" runEmailTemplateTests
#endif
  group "Persist relations" persistRelationsTests
  group "Data bridge" dataBridgeTests
  group "Feedback" feedbackTests
  group "Parse" parseTests
  group "Accept language" acceptLanguageTests
  group "Patch dictionaries" patchDictionariesTests

